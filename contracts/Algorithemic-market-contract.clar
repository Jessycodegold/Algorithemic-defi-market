;; Dynamic Market Maker - Fixed Version
;; A dynamic market maker that adjusts parameters based on market conditions

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-authorized (err u101))
(define-constant err-pool-exists (err u102))
(define-constant err-pool-not-found (err u103))
(define-constant err-token-exists (err u104))
(define-constant err-token-not-found (err u105))
(define-constant err-insufficient-balance (err u106))
(define-constant err-zero-amount (err u107))
(define-constant err-price-impact-too-high (err u108))
(define-constant err-slippage-too-high (err u109))
(define-constant err-insufficient-liquidity (err u110))
(define-constant err-invalid-parameters (err u111))
(define-constant err-shutdown-active (err u114))
(define-constant err-min-deposit (err u115))
(define-constant err-position-not-found (err u116))
(define-constant err-paused (err u117))
(define-constant err-transfer-failed (err u125))

;; Protocol parameters - renamed emergency-shutdown to avoid conflict
(define-data-var next-pool-id uint u1)
(define-data-var next-position-id uint u1)
(define-data-var protocol-fee-bp uint u30)
(define-data-var min-deposit-amount uint u1000000)
(define-data-var is-shutdown bool false)
(define-data-var treasury-address principal contract-owner)

;; Data maps
(define-map tokens
  { token-id: (string-ascii 20) }
  {
    name: (string-ascii 40),
    contract-address: principal,
    decimals: uint,
    is-stable: bool,
    last-price: uint,
    last-update-block: uint
  }
)

(define-map pools
  { pool-id: uint }
  {
    token-x: (string-ascii 20),
    token-y: (string-ascii 20),
    reserve-x: uint,
    reserve-y: uint,
    liquidity-units: uint,
    base-fee-bp: uint,
    total-volume-x: uint,
    total-volume-y: uint,
    creation-block: uint,
    last-update-block: uint,
    status: uint
  }
)

(define-map positions
  { position-id: uint }
  {
    pool-id: uint,
    provider: principal,
    liquidity-units: uint,
    token-x-amount: uint,
    token-y-amount: uint,
    entry-block: uint,
    fees-earned-x: uint,
    fees-earned-y: uint
  }
)

(define-map user-positions
  { user: principal }
  { count: uint }
)

(define-map user-position-list
  { user: principal, index: uint }
  { position-id: uint }
)

;; Private helper functions
(define-private (min-uint (a uint) (b uint))
  (if (<= a b) a b)
)

(define-private (is-owner)
  (is-eq tx-sender contract-owner)
)

(define-private (is-active)
  (not (var-get is-shutdown))
)

(define-private (mock-transfer (token (string-ascii 20)) (amount uint) (from principal) (to principal))
  (if (and (> amount u0) (not (is-eq from to)))
    (ok amount)
    err-transfer-failed
  )
)

;; Read-only functions
(define-read-only (get-pool-info (pool-id uint))
  (map-get? pools { pool-id: pool-id })
)

(define-read-only (get-position-info (position-id uint))
  (map-get? positions { position-id: position-id })
)

(define-read-only (get-token-info (token-id (string-ascii 20)))
  (map-get? tokens { token-id: token-id })
)

(define-read-only (get-stats)
  {
    next-pool-id: (var-get next-pool-id),
    next-position-id: (var-get next-position-id),
    protocol-fee-bp: (var-get protocol-fee-bp),
    min-deposit-amount: (var-get min-deposit-amount),
    is-shutdown: (var-get is-shutdown),
    treasury-address: (var-get treasury-address)
  }
)

;; Public functions - simplified to avoid interdependency
(define-public (set-emergency-shutdown (shutdown bool))
  (begin
    (asserts! (is-owner) err-owner-only)
    (var-set is-shutdown shutdown)
    (ok shutdown)
  )
)

(define-public (init-protocol (treasury principal))
  (begin
    (asserts! (is-owner) err-owner-only)
    (var-set treasury-address treasury)
    (var-set protocol-fee-bp u30)
    (var-set min-deposit-amount u1000000)
    (var-set is-shutdown false)
    (ok true)
  )
)

(define-public (add-token
  (token-id (string-ascii 20))
  (name (string-ascii 40))
  (contract-address principal)
  (decimals uint)
  (is-stable bool))
  (begin
    (asserts! (is-owner) err-owner-only)
    (asserts! (is-active) err-shutdown-active)
    (asserts! (is-none (map-get? tokens { token-id: token-id })) err-token-exists)
    
    (map-set tokens
      { token-id: token-id }
      {
        name: name,
        contract-address: contract-address,
        decimals: decimals,
        is-stable: is-stable,
        last-price: u0,
        last-update-block: block-height
      }
    )
    (ok token-id)
  )
)

(define-public (create-new-pool
  (token-x (string-ascii 20))
  (token-y (string-ascii 20))
  (base-fee-bp uint))
  (let (
    (pool-id (var-get next-pool-id))
  )
    (asserts! (is-owner) err-owner-only)
    (asserts! (is-active) err-shutdown-active)
    (asserts! (<= base-fee-bp u500) err-invalid-parameters)
    (asserts! (is-some (map-get? tokens { token-id: token-x })) err-token-not-found)
    (asserts! (is-some (map-get? tokens { token-id: token-y })) err-token-not-found)
    
    (map-set pools
      { pool-id: pool-id }
      {
        token-x: token-x,
        token-y: token-y,
        reserve-x: u0,
        reserve-y: u0,
        liquidity-units: u0,
        base-fee-bp: base-fee-bp,
        total-volume-x: u0,
        total-volume-y: u0,
        creation-block: block-height,
        last-update-block: block-height,
        status: u0
      }
    )
    
    (var-set next-pool-id (+ pool-id u1))
    (ok pool-id)
  )
)

(define-public (provide-liquidity
  (pool-id uint)
  (amount-x uint)
  (amount-y uint)
  (min-lp-units uint))
  (let (
    (provider tx-sender)
    (pool (unwrap! (map-get? pools { pool-id: pool-id }) err-pool-not-found))
    (position-id (var-get next-position-id))
    (current-liquidity (get liquidity-units pool))
    (reserve-x (get reserve-x pool))
    (reserve-y (get reserve-y pool))
  )
    (asserts! (is-active) err-shutdown-active)
    (asserts! (is-eq (get status pool) u0) err-paused)
    (asserts! (> amount-x u0) err-zero-amount)
    (asserts! (> amount-y u0) err-zero-amount)
    (asserts! (>= amount-x (var-get min-deposit-amount)) err-min-deposit)
    
    (let (
      (lp-units (if (is-eq current-liquidity u0)
                   (sqrti (* amount-x amount-y))
                   (min-uint
                     (/ (* amount-x current-liquidity) reserve-x)
                     (/ (* amount-y current-liquidity) reserve-y)
                   )))
    )
      (asserts! (>= lp-units min-lp-units) err-slippage-too-high)
      
      (try! (mock-transfer (get token-x pool) amount-x provider (as-contract tx-sender)))
      (try! (mock-transfer (get token-y pool) amount-y provider (as-contract tx-sender)))
      
      (map-set pools
        { pool-id: pool-id }
        (merge pool {
          reserve-x: (+ reserve-x amount-x),
          reserve-y: (+ reserve-y amount-y),
          liquidity-units: (+ current-liquidity lp-units),
          last-update-block: block-height
        })
      )
      
      (map-set positions
        { position-id: position-id }
        {
          pool-id: pool-id,
          provider: provider,
          liquidity-units: lp-units,
          token-x-amount: amount-x,
          token-y-amount: amount-y,
          entry-block: block-height,
          fees-earned-x: u0,
          fees-earned-y: u0
        }
      )
      
      (let (
        (user-count (default-to u0 (get count (map-get? user-positions { user: provider }))))
      )
        (map-set user-positions { user: provider } { count: (+ user-count u1) })
        (map-set user-position-list { user: provider, index: user-count } { position-id: position-id })
      )
      
      (var-set next-position-id (+ position-id u1))
      
      (ok { 
        position-id: position-id, 
        lp-units: lp-units
      })
    )
  )
)

(define-public (withdraw-liquidity
  (position-id uint)
  (lp-units uint)
  (min-amount-x uint)
  (min-amount-y uint))
  (let (
    (provider tx-sender)
    (position (unwrap! (map-get? positions { position-id: position-id }) err-position-not-found))
    (pool-id (get pool-id position))
    (pool (unwrap! (map-get? pools { pool-id: pool-id }) err-pool-not-found))
    (position-liquidity (get liquidity-units position))
  )
    (asserts! (is-active) err-shutdown-active)
    (asserts! (is-eq provider (get provider position)) err-not-authorized)
    (asserts! (> lp-units u0) err-zero-amount)
    (asserts! (<= lp-units position-liquidity) err-insufficient-balance)
    
    (let (
      (withdrawal-percentage (/ (* lp-units u10000) position-liquidity))
      (amount-x-return (/ (* (get token-x-amount position) withdrawal-percentage) u10000))
      (amount-y-return (/ (* (get token-y-amount position) withdrawal-percentage) u10000))
    )
      (asserts! (>= amount-x-return min-amount-x) err-slippage-too-high)
      (asserts! (>= amount-y-return min-amount-y) err-slippage-too-high)
      
      (try! (mock-transfer (get token-x pool) amount-x-return (as-contract tx-sender) provider))
      (try! (mock-transfer (get token-y pool) amount-y-return (as-contract tx-sender) provider))
      
      (if (is-eq lp-units position-liquidity)
        (map-delete positions { position-id: position-id })
        (map-set positions
          { position-id: position-id }
          (merge position {
            liquidity-units: (- position-liquidity lp-units),
            token-x-amount: (- (get token-x-amount position) amount-x-return),
            token-y-amount: (- (get token-y-amount position) amount-y-return)
          })
        )
      )
      
      (map-set pools
        { pool-id: pool-id }
        (merge pool {
          reserve-x: (- (get reserve-x pool) amount-x-return),
          reserve-y: (- (get reserve-y pool) amount-y-return),
          liquidity-units: (- (get liquidity-units pool) lp-units)
        })
      )
      
      (ok {
        amount-x: amount-x-return,
        amount-y: amount-y-return
      })
    )
  )
)

(define-public (execute-swap
  (pool-id uint)
  (token-in (string-ascii 20))
  (amount-in uint)
  (min-amount-out uint))
  (let (
    (pool (unwrap! (map-get? pools { pool-id: pool-id }) err-pool-not-found))
    (token-x (get token-x pool))
    (token-y (get token-y pool))
    (is-x-to-y (is-eq token-in token-x))
  )
    (asserts! (is-active) err-shutdown-active)
    (asserts! (is-eq (get status pool) u0) err-paused)
    (asserts! (> amount-in u0) err-zero-amount)
    (asserts! (or (is-eq token-in token-x) (is-eq token-in token-y)) err-invalid-parameters)
    
    (let (
      (reserve-in (if is-x-to-y (get reserve-x pool) (get reserve-y pool)))
      (reserve-out (if is-x-to-y (get reserve-y pool) (get reserve-x pool)))
      (fee-bp (get base-fee-bp pool))
      (amount-in-after-fee (- amount-in (/ (* amount-in fee-bp) u10000)))
      (amount-out (/ (* amount-in-after-fee reserve-out) (+ reserve-in amount-in-after-fee)))
    )
      (asserts! (>= amount-out min-amount-out) err-slippage-too-high)
      (asserts! (< amount-out reserve-out) err-insufficient-liquidity)
      
      (try! (mock-transfer token-in amount-in tx-sender (as-contract tx-sender)))
      (try! (mock-transfer (if is-x-to-y token-y token-x) amount-out (as-contract tx-sender) tx-sender))
      
      (map-set pools
        { pool-id: pool-id }
        (merge pool {
          reserve-x: (if is-x-to-y (+ (get reserve-x pool) amount-in) (- (get reserve-x pool) amount-out)),
          reserve-y: (if is-x-to-y (- (get reserve-y pool) amount-out) (+ (get reserve-y pool) amount-in)),
          total-volume-x: (if is-x-to-y (+ (get total-volume-x pool) amount-in) (+ (get total-volume-x pool) amount-out)),
          total-volume-y: (if is-x-to-y (+ (get total-volume-y pool) amount-out) (+ (get total-volume-y pool) amount-in)),
          last-update-block: block-height
        })
      )
      
      (ok {
        amount-in: amount-in,
        amount-out: amount-out,
        token-out: (if is-x-to-y token-y token-x)
      })
    )
  )
)