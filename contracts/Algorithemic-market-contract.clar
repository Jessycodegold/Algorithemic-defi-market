;; Dynamic Market Maker - Clarinet Compliant Version
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
(define-constant err-range-invalid (err u112))
(define-constant err-position-not-found (err u113))
(define-constant err-emergency-shutdown (err u114))
(define-constant err-min-deposit (err u115))
(define-constant err-position-still-active (err u116))
(define-constant err-paused (err u117))
(define-constant err-oracle-error (err u118))
(define-constant err-oracle-stale (err u119))
(define-constant err-unauthorized-oracle (err u120))
(define-constant err-invalid-curve (err u121))
(define-constant err-range-outside-pool (err u122))
(define-constant err-rewards-claimed-already (err u123))
(define-constant err-no-rewards-available (err u124))
(define-constant err-transfer-failed (err u125))

;; Protocol parameters
(define-data-var next-pool-id uint u1)
(define-data-var next-position-id uint u1)
(define-data-var protocol-fee-bp uint u30) ;; 0.3% base protocol fee in basis points
(define-data-var min-deposit-amount uint u1000000) ;; 1 STX minimum deposit
(define-data-var emergency-shutdown bool false)
(define-data-var treasury-address principal contract-owner)
(define-data-var volatility-update-frequency uint u144) ;; Update volatility approx once per day
(define-data-var max-price-impact-bp uint u300) ;; 3% max price impact
(define-data-var impermanent-loss-threshold uint u500) ;; 5% threshold for IL protection
(define-data-var impermanent-loss-coverage-bp uint u5000) ;; 50% IL coverage
(define-data-var dynamic-range-adjustment-factor uint u500) ;; 5% range adjustment
(define-data-var price-deviation-threshold uint u200) ;; 2% threshold for price deviation
(define-data-var max-dynamic-fee-increase uint u500) ;; Maximum 5% fee increase

;; Data maps
(define-map token-registry
  { token-id: (string-ascii 20) }
  {
    name: (string-ascii 40),
    token-type: (string-ascii 10), ;; "fungible" or "non-fungible"
    contract-address: principal,
    decimals: uint,
    price-oracle: principal,
    current-volatility: uint, ;; Current volatility score 1-10000
    is-stable: bool, ;; Is this a stablecoin
    max-supply: uint,
    last-price: uint, ;; Last price in STX with 8 decimals
    last-update-block: uint
  }
)

(define-map price-history-entries
  { pool-id: uint, index: uint }
  { price: uint, timestamp: uint }
)

(define-map concentrated-ranges
  { pool-id: uint, range-id: uint }
  { 
    tick-lower: int, 
    tick-upper: int, 
    liquidity: uint, 
    positions-count: uint 
  }
)

(define-map liquidity-pools
  { pool-id: uint }
  {
    token-x: (string-ascii 20),
    token-y: (string-ascii 20),
    reserve-x: uint,
    reserve-y: uint,
    virtual-reserve-x: uint, ;; Used for stableswap and custom curves
    virtual-reserve-y: uint,
    liquidity-units: uint, ;; Total liquidity shares
    curve-type: uint,
    curve-param-1: uint, ;; First curve parameter
    curve-param-2: uint, ;; Second curve parameter
    curve-param-3: uint, ;; Third curve parameter
    curve-param-4: uint, ;; Fourth curve parameter
    curve-param-5: uint, ;; Fifth curve parameter
    base-fee-bp: uint, ;; Base fee in basis points
    dynamic-fee-bp: uint, ;; Additional dynamic fee based on volatility
    current-tick: int, ;; Current price tick (log base 1.0001 of price)
    tick-spacing: uint, ;; Minimum tick movement
    price-oracle: principal,
    total-volume-x: uint,
    total-volume-y: uint,
    total-fees-x: uint,
    total-fees-y: uint,
    total-fees-protocol: uint,
    creation-block: uint,
    last-update-block: uint,
    status: uint, ;; 0=Active, 1=Paused, 2=Deprecated
    price-history-count: uint, ;; Number of price history entries
    volatility-adjustment: uint, ;; Dynamic adjustment to fee and ranges
    range-count: uint, ;; Number of concentrated ranges
    total-il-compensation-paid: uint
  }
)

(define-map liquidity-positions
  { position-id: uint }
  {
    pool-id: uint,
    provider: principal,
    liquidity-units: uint, ;; Share of the pool
    token-x-amount: uint,
    token-y-amount: uint,
    entry-price: uint, ;; Entry price for impermanent loss calculation
    entry-sqrt-price: uint, ;; Square root of price at entry (for concentrated liquidity)
    entry-block: uint,
    last-update-block: uint,
    tick-lower: int, ;; Lower tick bound for concentrated liquidity
    tick-upper: int, ;; Upper tick bound for concentrated liquidity
    range-status: uint, ;; 0=Out-of-range, 1=In-range, 2=Partial-range
    fees-earned-x: uint,
    fees-earned-y: uint,
    rewards-earned: uint,
    rewards-claimed: uint,
    il-compensation: uint,
    is-concentrated: bool ;; Is this a concentrated liquidity position
  }
)

(define-map user-position-count
  { user: principal }
  { count: uint }
)

(define-map user-position-at-index
  { user: principal, index: uint }
  { position-id: uint }
)

(define-map pool-position-count
  { pool-id: uint }
  { count: uint }
)

(define-map pool-position-at-index
  { pool-id: uint, index: uint }
  { position-id: uint }
)

(define-map oracle-prices
  { token-id: (string-ascii 20) }
  {
    price: uint, ;; Price in STX with 8 decimals
    last-update-block: uint,
    twap-price: uint, ;; Time-weighted average price
    trusted: bool,
    oracle-address: principal
  }
)

;; Helper functions (must be defined before public functions)
(define-private (min-uint (a uint) (b uint))
  (if (<= a b) a b)
)

(define-private (is-authorized-caller)
  (is-eq tx-sender contract-owner)
)

(define-private (calculate-price-from-reserves (reserve-x uint) (reserve-y uint))
  (if (> reserve-x u0)
    (/ (* reserve-y u100000000) reserve-x)
    u0
  )
)

(define-private (calculate-sqrt-price-from-tick (tick int))
  ;; Simplified calculation - in production this would use proper tick math
  (if (>= tick 0)
    (+ u100000000 (* (to-uint tick) u1000))
    (if (>= (- 0 tick) 100)
      u1000000
      (- u100000000 (* (to-uint (- 0 tick)) u1000))
    )
  )
)

(define-private (calculate-concentrated-liquidity 
  (amount-x uint) 
  (amount-y uint) 
  (price-sqrt uint) 
  (lower-price-sqrt uint) 
  (upper-price-sqrt uint))
  ;; Simplified calculation for concentrated liquidity
  (let (
    (liquidity-x (if (> upper-price-sqrt price-sqrt)
                   (/ (* amount-x price-sqrt) (- upper-price-sqrt price-sqrt))
                   u0))
    (liquidity-y (if (> price-sqrt lower-price-sqrt)
                   (/ amount-y (- price-sqrt lower-price-sqrt))
                   u0))
  )
    (if (> liquidity-x u0)
      (if (> liquidity-y u0)
        (min-uint liquidity-x liquidity-y)
        liquidity-x)
      liquidity-y)
  )
)

(define-private (add-user-position (user principal) (position-id uint))
  (let (
    (current-count (default-to u0 (get count (map-get? user-position-count { user: user }))))
  )
    (map-set user-position-count { user: user } { count: (+ current-count u1) })
    (map-set user-position-at-index { user: user, index: current-count } { position-id: position-id })
    (ok true)
  )
)

(define-private (add-pool-position (pool-id uint) (position-id uint))
  (let (
    (current-count (default-to u0 (get count (map-get? pool-position-count { pool-id: pool-id }))))
  )
    (map-set pool-position-count { pool-id: pool-id } { count: (+ current-count u1) })
    (map-set pool-position-at-index { pool-id: pool-id, index: current-count } { position-id: position-id })
    (ok true)
  )
)

;; Fixed transfer function that can return an error
(define-private (transfer-token (token-id (string-ascii 20)) (amount uint) (from principal) (to principal))
  ;; This is a placeholder - in production you would call the actual token contract
  ;; For now, we'll simulate a transfer that could fail
  ;; In production, this would be something like:
  ;; (contract-call? .token-contract transfer amount from to none)
  (if (> amount u0)
    (ok true)
    err-transfer-failed
  )
)

;; Public functions
(define-public (initialize (treasury principal))
  (begin
    (asserts! (is-authorized-caller) err-owner-only)
    (var-set treasury-address treasury)
    (var-set protocol-fee-bp u30) ;; 0.3%
    (var-set min-deposit-amount u1000000) ;; 1 STX
    (var-set emergency-shutdown false)
    (ok true)
  )
)

(define-public (register-token
  (token-id (string-ascii 20))
  (name (string-ascii 40))
  (token-type (string-ascii 10))
  (contract-address principal)
  (decimals uint)
  (price-oracle principal)
  (is-stable bool)
  (max-supply uint))
  
  (begin
    (asserts! (is-authorized-caller) err-owner-only)
    (asserts! (is-eq (get status pool) u0) err-paused)
    (asserts! (> amount-in u0) err-zero-amount)
    (asserts! (or (is-eq token-in token-x) (is-eq token-in token-y)) err-token-not-found)
    
    (let (
      (reserve-in (if is-x-to-y (get reserve-x pool) (get reserve-y pool)))
      (reserve-out (if is-x-to-y (get reserve-y pool) (get reserve-x pool)))
      (total-fee-bp (+ (get base-fee-bp pool) (get dynamic-fee-bp pool)))
      (amount-in-after-fee (- amount-in (/ (* amount-in total-fee-bp) u10000)))
      (amount-out (/ (* amount-in-after-fee reserve-out) (+ reserve-in amount-in-after-fee)))
      (fee-amount (/ (* amount-in total-fee-bp) u10000))
      (protocol-fee (/ (* fee-amount (var-get protocol-fee-bp)) u10000))
    )
      ;; Check slippage
      (asserts! (>= amount-out min-amount-out) err-slippage-too-high)
      
      ;; Check sufficient liquidity
      (asserts! (< amount-out reserve-out) err-insufficient-liquidity)
      
      ;; Calculate price impact
      (let (
        (price-before (/ (* reserve-out u10000) reserve-in))
        (new-reserve-in (+ reserve-in amount-in))
        (new-reserve-out (- reserve-out amount-out))
        (price-after (/ (* new-reserve-out u10000) new-reserve-in))
        (price-impact (if (> price-after price-before)
                        (/ (* (- price-after price-before) u10000) price-before)
                        (/ (* (- price-before price-after) u10000) price-before)))
      )
        ;; Check price impact limit
        (asserts! (<= price-impact (var-get max-price-impact-bp)) err-price-impact-too-high)
        
        ;; Execute swap
        (try! (transfer-token token-in amount-in tx-sender (as-contract tx-sender)))
        (try! (transfer-token (if is-x-to-y token-y token-x) amount-out (as-contract tx-sender) tx-sender))
        
        ;; Update pool state
        (map-set liquidity-pools
          { pool-id: pool-id }
          (merge pool {
            reserve-x: (if is-x-to-y new-reserve-in new-reserve-out),
            reserve-y: (if is-x-to-y new-reserve-out new-reserve-in),
            total-volume-x: (+ (get total-volume-x pool) (if is-x-to-y amount-in u0)),
            total-volume-y: (+ (get total-volume-y pool) (if is-x-to-y u0 amount-in)),
            total-fees-x: (+ (get total-fees-x pool) (if is-x-to-y (- fee-amount protocol-fee) u0)),
            total-fees-y: (+ (get total-fees-y pool) (if is-x-to-y u0 (- fee-amount protocol-fee))),
            total-fees-protocol: (+ (get total-fees-protocol pool) protocol-fee),
            last-update-block: block-height
          })
        )
        
        (ok {
          amount-in: amount-in,
          amount-out: amount-out,
          fee-amount: fee-amount,
          price-impact: price-impact
        })
      )
    )
  )
)

(define-public (update-oracle-price
  (token-id (string-ascii 20))
  (new-price uint))
  
  (let (
    (token-info (unwrap! (map-get? token-registry { token-id: token-id }) err-token-not-found))
    (oracle-info (unwrap! (map-get? oracle-prices { token-id: token-id }) err-oracle-error))
  )
    ;; Only authorized oracle can update
    (asserts! (is-eq tx-sender (get oracle-address oracle-info)) err-unauthorized-oracle)
    (asserts! (> new-price u0) err-invalid-parameters)
    
    ;; Calculate TWAP (simplified)
    (let (
      (old-price (get price oracle-info))
      (time-diff (- block-height (get last-update-block oracle-info)))
      (twap-price (if (> time-diff u0)
                    (/ (+ (* old-price time-diff) new-price) (+ time-diff u1))
                    new-price))
    )
      ;; Update oracle data
      (map-set oracle-prices
        { token-id: token-id }
        (merge oracle-info {
          price: new-price,
          last-update-block: block-height,
          twap-price: twap-price
        })
      )
      
      ;; Update token registry
      (map-set token-registry
        { token-id: token-id }
        (merge token-info {
          last-price: new-price,
          last-update-block: block-height
        })
      )
      
      (ok new-price)
    )
  )
)

(define-public (emergency-shutdown)
  (begin
    (asserts! (is-authorized-caller) err-owner-only)
    (var-set emergency-shutdown true)
    (ok true)
  )
)

(define-public (emergency-resume)
  (begin
    (asserts! (is-authorized-caller) err-owner-only)
    (var-set emergency-shutdown false)
    (ok true)
  )
)

(define-public (pause-pool (pool-id uint))
  (let (
    (pool (unwrap! (map-get? liquidity-pools { pool-id: pool-id }) err-pool-not-found))
  )
    (asserts! (is-authorized-caller) err-owner-only)
    (map-set liquidity-pools
      { pool-id: pool-id }
      (merge pool { status: u1 }) ;; Set to paused
    )
    (ok true)
  )
)

(define-public (resume-pool (pool-id uint))
  (let (
    (pool (unwrap! (map-get? liquidity-pools { pool-id: pool-id }) err-pool-not-found))
  )
    (asserts! (is-authorized-caller) err-owner-only)
    (map-set liquidity-pools
      { pool-id: pool-id }
      (merge pool { status: u0 }) ;; Set to active
    )
    (ok true)
  )
)

;; Read-only functions
(define-read-only (get-pool-info (pool-id uint))
  (map-get? liquidity-pools { pool-id: pool-id })
)

(define-read-only (get-position-info (position-id uint))
  (map-get? liquidity-positions { position-id: position-id })
)

(define-read-only (get-token-info (token-id (string-ascii 20)))
  (map-get? token-registry { token-id: token-id })
)

(define-read-only (get-oracle-price (token-id (string-ascii 20)))
  (map-get? oracle-prices { token-id: token-id })
)

(define-read-only (get-user-position-count (user principal))
  (default-to u0 (get count (map-get? user-position-count { user: user })))
)

(define-read-only (get-user-position-at-index (user principal) (index uint))
  (map-get? user-position-at-index { user: user, index: index })
)

(define-read-only (get-pool-position-count (pool-id uint))
  (default-to u0 (get count (map-get? pool-position-count { pool-id: pool-id })))
)

(define-read-only (get-pool-position-at-index (pool-id uint) (index uint))
  (map-get? pool-position-at-index { pool-id: pool-id, index: index })
)

(define-read-only (get-concentrated-range (pool-id uint) (range-id uint))
  (map-get? concentrated-ranges { pool-id: pool-id, range-id: range-id })
)

(define-read-only (get-price-history-entry (pool-id uint) (index uint))
  (map-get? price-history-entries { pool-id: pool-id, index: index })
)

(define-read-only (calculate-swap-output
  (pool-id uint)
  (token-in (string-ascii 20))
  (amount-in uint))
  
  (match (map-get? liquidity-pools { pool-id: pool-id })
    pool (let (
      (token-x (get token-x pool))
      (token-y (get token-y pool))
      (is-x-to-y (is-eq token-in token-x))
      (reserve-in (if is-x-to-y (get reserve-x pool) (get reserve-y pool)))
      (reserve-out (if is-x-to-y (get reserve-y pool) (get reserve-x pool)))
      (total-fee-bp (+ (get base-fee-bp pool) (get dynamic-fee-bp pool)))
      (amount-in-after-fee (- amount-in (/ (* amount-in total-fee-bp) u10000)))
      (amount-out (/ (* amount-in-after-fee reserve-out) (+ reserve-in amount-in-after-fee)))
    )
      (ok {
        amount-out: amount-out,
        fee-amount: (/ (* amount-in total-fee-bp) u10000),
        price-impact: (/ (* amount-out u10000) reserve-out)
      })
    )
    err-pool-not-found
  )
)

(define-read-only (get-protocol-stats)
  {
    next-pool-id: (var-get next-pool-id),
    next-position-id: (var-get next-position-id),
    protocol-fee-bp: (var-get protocol-fee-bp),
    min-deposit-amount: (var-get min-deposit-amount),
    emergency-shutdown: (var-get emergency-shutdown),
    treasury-address: (var-get treasury-address)
  }
)
    (asserts! (is-none (map-get? token-registry { token-id: token-id })) err-token-exists)
    
    ;; Create token registry entry
    (map-set token-registry
      { token-id: token-id }
      {
        name: name,
        token-type: token-type,
        contract-address: contract-address,
        decimals: decimals,
        price-oracle: price-oracle,
        current-volatility: u1000, ;; Start with medium volatility (10%)
        is-stable: is-stable,
        max-supply: max-supply,
        last-price: u0,
        last-update-block: block-height
      }
    )
    
    ;; Initialize oracle entry
    (map-set oracle-prices
      { token-id: token-id }
      {
        price: u0,
        last-update-block: block-height,
        twap-price: u0,
        trusted: true,
        oracle-address: price-oracle
      }
    )
    
    (ok token-id)
  )
)

(define-public (create-pool
  (token-x (string-ascii 20))
  (token-y (string-ascii 20))
  (curve-type uint)
  (curve-param-1 uint)
  (curve-param-2 uint)
  (curve-param-3 uint)
  (curve-param-4 uint)
  (curve-param-5 uint)
  (base-fee-bp uint)
  (tick-spacing uint))
  
  (let (
    (pool-id (var-get next-pool-id))
    (token-x-info (unwrap! (map-get? token-registry { token-id: token-x }) err-token-not-found))
    (token-y-info (unwrap! (map-get? token-registry { token-id: token-y }) err-token-not-found))
  )
    (asserts! (is-authorized-caller) err-owner-only)
    (asserts! (not (var-get emergency-shutdown)) err-emergency-shutdown)
    (asserts! (< curve-type u4) err-invalid-curve) ;; Valid curve type
    (asserts! (<= base-fee-bp u500) err-invalid-parameters) ;; Max 5% base fee
    (asserts! (> tick-spacing u0) err-invalid-parameters) ;; Tick spacing must be positive
    
    ;; Create pool
    (map-set liquidity-pools
      { pool-id: pool-id }
      {
        token-x: token-x,
        token-y: token-y,
        reserve-x: u0,
        reserve-y: u0,
        virtual-reserve-x: u0,
        virtual-reserve-y: u0,
        liquidity-units: u0,
        curve-type: curve-type,
        curve-param-1: curve-param-1,
        curve-param-2: curve-param-2,
        curve-param-3: curve-param-3,
        curve-param-4: curve-param-4,
        curve-param-5: curve-param-5,
        base-fee-bp: base-fee-bp,
        dynamic-fee-bp: u0, ;; Start with no dynamic fee
        current-tick: 0, ;; Start at price = 1.0
        tick-spacing: tick-spacing,
        price-oracle: (get price-oracle token-x-info), ;; Use token X's oracle by default
        total-volume-x: u0,
        total-volume-y: u0,
        total-fees-x: u0,
        total-fees-y: u0,
        total-fees-protocol: u0,
        creation-block: block-height,
        last-update-block: block-height,
        status: u0, ;; Active
        price-history-count: u0,
        volatility-adjustment: u1000, ;; Start with 10% volatility adjustment
        range-count: u0,
        total-il-compensation-paid: u0
      }
    )
    
    ;; Initialize pool positions count
    (map-set pool-position-count
      { pool-id: pool-id }
      { count: u0 }
    )
    
    ;; Increment pool ID counter
    (var-set next-pool-id (+ pool-id u1))
    
    (ok { pool-id: pool-id })
  )
)

(define-public (add-liquidity
  (pool-id uint)
  (amount-x uint)
  (amount-y uint)
  (min-lp-units uint))
  
  (let (
    (provider tx-sender)
    (pool (unwrap! (map-get? liquidity-pools { pool-id: pool-id }) err-pool-not-found))
    (token-x (get token-x pool))
    (token-y (get token-y pool))
    (position-id (var-get next-position-id))
  )
    ;; Validation
    (asserts! (not (var-get emergency-shutdown)) err-emergency-shutdown)
    (asserts! (is-eq (get status pool) u0) err-paused) ;; Pool must be active
    (asserts! (> amount-x u0) err-zero-amount)
    (asserts! (> amount-y u0) err-zero-amount)
    (asserts! (>= amount-x (var-get min-deposit-amount)) err-min-deposit) ;; Minimum deposit
    (asserts! (>= amount-y (var-get min-deposit-amount)) err-min-deposit) ;; Minimum deposit
    
    ;; Calculate liquidity units (LP tokens)
    (let (
      (current-liquidity (get liquidity-units pool))
      (reserve-x (get reserve-x pool))
      (reserve-y (get reserve-y pool))
      (lp-units (if (is-eq current-liquidity u0)
                   ;; First liquidity provision - use geometric mean
                   (sqrti (* amount-x amount-y))
                   ;; Proportional to existing reserves
                   (min-uint
                     (/ (* amount-x current-liquidity) reserve-x)
                     (/ (* amount-y current-liquidity) reserve-y)
                   )))
    )
      ;; Ensure minimum liquidity
      (asserts! (>= lp-units min-lp-units) err-slippage-too-high)
      
      ;; Transfer tokens to pool
      (try! (transfer-token token-x amount-x provider (as-contract tx-sender)))
      (try! (transfer-token token-y amount-y provider (as-contract tx-sender)))
      
      ;; Update pool state
      (map-set liquidity-pools
        { pool-id: pool-id }
        (merge pool {
          reserve-x: (+ reserve-x amount-x),
          reserve-y: (+ reserve-y amount-y),
          liquidity-units: (+ current-liquidity lp-units),
          last-update-block: block-height
        })
      )
      
      ;; Create liquidity position
      (map-set liquidity-positions
        { position-id: position-id }
        {
          pool-id: pool-id,
          provider: provider,
          liquidity-units: lp-units,
          token-x-amount: amount-x,
          token-y-amount: amount-y,
          entry-price: (calculate-price-from-reserves reserve-y reserve-x),
          entry-sqrt-price: (sqrti (/ reserve-y reserve-x)),
          entry-block: block-height,
          last-update-block: block-height,
          tick-lower: 0, ;; Full range
          tick-upper: 0, ;; Full range
          range-status: u1, ;; In-range
          fees-earned-x: u0,
          fees-earned-y: u0,
          rewards-earned: u0,
          rewards-claimed: u0,
          il-compensation: u0,
          is-concentrated: false
        }
      )
      
      ;; Update user's positions list
      (try! (add-user-position provider position-id))
      
      ;; Update pool's positions list
      (try! (add-pool-position pool-id position-id))
      
      ;; Increment position ID counter
      (var-set next-position-id (+ position-id u1))
      
      (ok { 
        position-id: position-id, 
        lp-units: lp-units,
        amount-x: amount-x,
        amount-y: amount-y 
      })
    )
  )
)

(define-public (add-concentrated-liquidity
  (pool-id uint)
  (amount-x uint)
  (amount-y uint)
  (tick-lower int)
  (tick-upper int)
  (min-lp-units uint))
  
  (let (
    (provider tx-sender)
    (pool (unwrap! (map-get? liquidity-pools { pool-id: pool-id }) err-pool-not-found))
    (token-x (get token-x pool))
    (token-y (get token-y pool))
    (position-id (var-get next-position-id))
    (current-tick (get current-tick pool))
    (tick-spacing (get tick-spacing pool))
  )
    ;; Validation
    (asserts! (not (var-get emergency-shutdown)) err-emergency-shutdown)
    (asserts! (is-eq (get status pool) u0) err-paused) ;; Pool must be active
    (asserts! (> amount-x u0) err-zero-amount)
    (asserts! (> amount-y u0) err-zero-amount)
    (asserts! (>= amount-x (var-get min-deposit-amount)) err-min-deposit) ;; Minimum deposit
    (asserts! (>= amount-y (var-get min-deposit-amount)) err-min-deposit) ;; Minimum deposit
    (asserts! (< tick-lower tick-upper) err-range-invalid) ;; Valid range
    
    ;; Ensure ticks are on the spacing grid
    (asserts! (is-eq (mod tick-lower (to-int tick-spacing)) 0) err-invalid-parameters)
    (asserts! (is-eq (mod tick-upper (to-int tick-spacing)) 0) err-invalid-parameters)
    
    ;; Calculate liquidity provision based on range
    (let (
      (price-sqrt (sqrti (/ (get reserve-y pool) (get reserve-x pool))))
      (lower-price-sqrt (calculate-sqrt-price-from-tick tick-lower))
      (upper-price-sqrt (calculate-sqrt-price-from-tick tick-upper))
      (is-current-in-range (and (>= current-tick tick-lower) (< current-tick tick-upper)))
      (range-status (if is-current-in-range u1 u0)) ;; 1=In-range, 0=Out-of-range
      (lp-units (calculate-concentrated-liquidity amount-x amount-y price-sqrt lower-price-sqrt upper-price-sqrt))
      (current-range-count (get range-count pool))
    )
      ;; Ensure minimum liquidity
      (asserts! (>= lp-units min-lp-units) err-slippage-too-high)
      
      ;; Transfer tokens to pool
      (try! (transfer-token token-x amount-x provider (as-contract tx-sender)))
      (try! (transfer-token token-y amount-y provider (as-contract tx-sender)))
      
      ;; Add concentrated range
      (map-set concentrated-ranges
        { pool-id: pool-id, range-id: current-range-count }
        {
          tick-lower: tick-lower,
          tick-upper: tick-upper,
          liquidity: lp-units,
          positions-count: u1
        }
      )
      
      ;; Update pool state
      (map-set liquidity-pools
        { pool-id: pool-id }
        (merge pool {
          reserve-x: (+ (get reserve-x pool) amount-x),
          reserve-y: (+ (get reserve-y pool) amount-y),
          liquidity-units: (+ (get liquidity-units pool) lp-units),
          last-update-block: block-height,
          range-count: (+ current-range-count u1)
        })
      )
      
      ;; Create liquidity position
      (map-set liquidity-positions
        { position-id: position-id }
        {
          pool-id: pool-id,
          provider: provider,
          liquidity-units: lp-units,
          token-x-amount: amount-x,
          token-y-amount: amount-y,
          entry-price: (calculate-price-from-reserves (get reserve-y pool) (get reserve-x pool)),
          entry-sqrt-price: price-sqrt,
          entry-block: block-height,
          last-update-block: block-height,
          tick-lower: tick-lower,
          tick-upper: tick-upper,
          range-status: range-status,
          fees-earned-x: u0,
          fees-earned-y: u0,
          rewards-earned: u0,
          rewards-claimed: u0,
          il-compensation: u0,
          is-concentrated: true
        }
      )
      
      ;; Update user's positions list
      (try! (add-user-position provider position-id))
      
      ;; Update pool's positions list
      (try! (add-pool-position pool-id position-id))
      
      ;; Increment position ID counter
      (var-set next-position-id (+ position-id u1))
      
      (ok { 
        position-id: position-id, 
        lp-units: lp-units,
        amount-x: amount-x,
        amount-y: amount-y,
        range-status: range-status
      })
    )
  )
)

(define-public (remove-liquidity
  (position-id uint)
  (lp-units uint)
  (min-amount-x uint)
  (min-amount-y uint))
  
  (let (
    (provider tx-sender)
    (position (unwrap! (map-get? liquidity-positions { position-id: position-id }) err-position-not-found))
    (pool-id (get pool-id position))
    (pool (unwrap! (map-get? liquidity-pools { pool-id: pool-id }) err-pool-not-found))
    (token-x (get token-x pool))
    (token-y (get token-y pool))
  )
    ;; Validation
    (asserts! (not (var-get emergency-shutdown)) err-emergency-shutdown)
    (asserts! (is-eq provider (get provider position)) err-not-authorized)
    (asserts! (> lp-units u0) err-zero-amount)
    (asserts! (<= lp-units (get liquidity-units position)) err-insufficient-balance)
    
    ;; Calculate amounts to return
    (let (
      (position-liquidity (get liquidity-units position))
      (withdrawal-percentage (/ (* lp-units u10000) position-liquidity))
      (amount-x-position (get token-x-amount position))
      (amount-y-position (get token-y-amount position))
      (amount-x-to-return (/ (* amount-x-position withdrawal-percentage) u10000))
      (amount-y-to-return (/ (* amount-y-position withdrawal-percentage) u10000))
      (fees-earned-x (get fees-earned-x position))
      (fees-earned-y (get fees-earned-y position))
      (fees-x-to-return (/ (* fees-earned-x withdrawal-percentage) u10000))
      (fees-y-to-return (/ (* fees-earned-y withdrawal-percentage) u10000))
      (il-compensation (get il-compensation position))
      (il-to-return (/ (* il-compensation withdrawal-percentage) u10000))
      (total-x-return (+ amount-x-to-return fees-x-to-return))
      (total-y-return (+ amount-y-to-return fees-y-to-return))
    )
      ;; Check slippage
      (asserts! (>= total-x-return min-amount-x) err-slippage-too-high)
      (asserts! (>= total-y-return min-amount-y) err-slippage-too-high)
      
      ;; Transfer tokens back to provider
      (try! (transfer-token token-x total-x-return (as-contract tx-sender) provider))
      (try! (transfer-token token-y total-y-return (as-contract tx-sender) provider))
      
      ;; Update position or remove if fully withdrawn
      (if (is-eq lp-units position-liquidity)
        ;; Full withdrawal - remove position
        (map-delete liquidity-positions { position-id: position-id })
        ;; Partial withdrawal - update position
        (map-set liquidity-positions
          { position-id: position-id }
          (merge position {
            liquidity-units: (- position-liquidity lp-units),
            token-x-amount: (- amount-x-position amount-x-to-return),
            token-y-amount: (- amount-y-position amount-y-to-return),
            fees-earned-x: (- fees-earned-x fees-x-to-return),
            fees-earned-y: (- fees-earned-y fees-y-to-return),
            il-compensation: (- il-compensation il-to-return),
            last-update-block: block-height
          })
        )
      )
      
      ;; Update pool reserves
      (map-set liquidity-pools
        { pool-id: pool-id }
        (merge pool {
          reserve-x: (- (get reserve-x pool) amount-x-to-return),
          reserve-y: (- (get reserve-y pool) amount-y-to-return),
          liquidity-units: (- (get liquidity-units pool) lp-units),
          total-fees-x: (- (get total-fees-x pool) fees-x-to-return),
          total-fees-y: (- (get total-fees-y pool) fees-y-to-return),
          last-update-block: block-height
        })
      )
      
      (ok {
        amount-x: total-x-return,
        amount-y: total-y-return,
        fees-x: fees-x-to-return,
        fees-y: fees-y-to-return,
        il-compensation: il-to-return
      })
    )
  )
)

(define-public (swap
  (pool-id uint)
  (token-in (string-ascii 20))
  (amount-in uint)
  (min-amount-out uint))
  
  (let (
    (pool (unwrap! (map-get? liquidity-pools { pool-id: pool-id }) err-pool-not-found))
    (token-x (get token-x pool))
    (token-y (get token-y pool))
    (is-x-to-y (is-eq token-in token-x))
  )
    ;; Validation
    (asserts! (not (var-get emergency-shutdown)) err-emergency-shutdown)