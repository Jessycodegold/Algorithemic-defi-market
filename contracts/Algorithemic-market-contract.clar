;; Algorithemic-market-contract
;; A dynamic market maker that adjusts parameters based on market conditions

;; constants
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

;; Curve types enumeration
;; 0 = Constant Product (x*y=k), 1 = Stableswap, 2 = Exponential, 3 = Dynamic
(define-data-var curve-types (list 4 (string-ascii 20)) (list "ConstantProduct" "Stableswap" "Exponential" "Dynamic"))

;; Pool status enumeration
;; 0 = Active, 1 = Paused, 2 = Deprecated
(define-data-var pool-statuses (list 3 (string-ascii 10)) (list "Active" "Paused" "Deprecated"))

;; Range status enumeration
;; 0 = Out-of-range, 1 = In-range, 2 = Partial-range
(define-data-var range-statuses (list 3 (string-ascii 15)) (list "Out-of-range" "In-range" "Partial-range"))

;; Supported tokens
(define-map token-registry
  { token-id: (string-ascii 20) }
  {
    name: (string-ascii 40),
    token-type: (string-ascii 10), ;; "fungible" or "non-fungible"
    contract: principal,
    decimals: uint,
    price-oracle: principal,
    volatility-history: (list 30 uint), ;; Recent volatility measurements
    current-volatility: uint, ;; Current volatility score 1-10000
    is-stable: bool, ;; Is this a stablecoin
    max-supply: uint,
    last-price: uint, ;; Last price in STX with 8 decimals
    last-update-block: uint
  }
)
;; Liquidity pools
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
    curve-params: (list 5 uint), ;; Custom parameters for the curve
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
    price-history: (list 24 { price: uint, timestamp: uint }), ;; 24 hours of price history
    volatility-adjustment: uint, ;; Dynamic adjustment to fee and ranges
    concentrated-ranges: (list 10 { 
      tick-lower: int, 
      tick-upper: int, 
      liquidity: uint, 
      positions-count: uint 
    }),
    total-il-compensation-paid: uint
  }
)

;; Liquidity positions
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

;; User positions index
(define-map user-positions
  { user: principal }
  { position-ids: (list 100 uint) }
)

;; Pool positions index
(define-map pool-positions
  { pool-id: uint }
  { position-ids: (list 500 uint) }
)

;; Oracle price data
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

;; Initialize the protocol
(define-public (initialize (treasury principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set treasury-address treasury)
    (var-set protocol-fee-bp u30) ;; 0.3%
    (var-set min-deposit-amount u1000000) ;; 1 STX
    (var-set emergency-shutdown false)
    (ok true)
  )
)

;; Register a token
(define-public (register-token
  (token-id (string-ascii 20))
  (name (string-ascii 40))
  (token-type (string-ascii 10))
  (contract principal)
  (decimals uint)
  (price-oracle principal)
  (is-stable bool)
  (max-supply uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (not (var-get emergency-shutdown)) err-emergency-shutdown)
    (asserts! (is-none (map-get? token-registry { token-id: token-id })) err-token-exists)
    ;; Create token registry entry
    (map-set token-registry
      { token-id: token-id }
      {
        name: name,
        token-type: token-type,
        contract: contract,
        decimals: decimals,
        price-oracle: price-oracle,
        volatility-history: (list u0 u0 u0 u0 u0 u0 u0 u0 u0 u0),
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

;; Create a new liquidity pool
(define-public (create-pool
  (token-x (string-ascii 20))
  (token-y (string-ascii 20))
  (curve-type uint)
  (curve-params (list 5 uint))
  (base-fee-bp uint)
  (tick-spacing uint))
  (let (
    (pool-id (var-get next-pool-id))
    (token-x-info (unwrap! (map-get? token-registry { token-id: token-x }) err-token-not-found))
    (token-y-info (unwrap! (map-get? token-registry { token-id: token-y }) err-token-not-found))
  )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
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
        curve-params: curve-params,
        base-fee-bp: base-fee-bp,
        dynamic-fee-bp: u0, ;; Start with no dynamic fee
        current-tick: (convert-to-int 0), ;; Start at price = 1.0
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
        price-history: (list),
        volatility-adjustment: u1000, ;; Start with 10% volatility adjustment
        concentrated-ranges: (list),
        total-il-compensation-paid: u0
      }
    )
    ;; Initialize pool positions list
    (map-set pool-positions
      { pool-id: pool-id }
      { position-ids: (list) }
    )
    ;; Increment pool ID counter
    (var-set next-pool-id (+ pool-id u1))
    (ok { pool-id: pool-id })
  )
)

;; Add standard liquidity to a pool
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
    (token-x-info (unwrap! (map-get? token-registry { token-id: token-x }) err-token-not-found))
    (token-y-info (unwrap! (map-get? token-registry { token-id: token-y }) err-token-not-found))
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
                   (min
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
          entry-price: (calculate-price pool),
          entry-sqrt-price: (sqrti (/ reserve-y reserve-x)),
          entry-block: block-height,
          last-update-block: block-height,
          tick-lower: (convert-to-int 0), ;; Full range
          tick-upper: (convert-to-int 0), ;; Full range
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
      (let (
        (user-pos (default-to { position-ids: (list) } (map-get? user-positions { user: provider })))
        (updated-user-pos (merge user-pos {
          position-ids: (append (get position-ids user-pos) position-id)
        }))
      )
        (map-set user-positions
          { user: provider }
          updated-user-pos
        )
      )
      ;; Update pool's positions list
      (let (
        (pool-pos (default-to { position-ids: (list) } (map-get? pool-positions { pool-id: pool-id })))
        (updated-pool-pos (merge pool-pos {
          position-ids: (append (get position-ids pool-pos) position-id)
        }))
      )
        (map-set pool-positions
          { pool-id: pool-id }
          updated-pool-pos
        )
      )
      ;; Increment position ID counter
      (var-set next-position-id (+ position-id u1))
      (ok { position-id: position-id })
    )
  )
)

;; Calculate current price of the pool
(define-public (calculate-price (pool-id uint))
  (let (
    (pool (unwrap! (map-get? liquidity-pools { pool-id: pool-id }) err-pool-not-found))
    (reserve-x (get reserve-x pool))
    (reserve-y (get reserve-y pool))
    (current-tick (get current-tick pool))
    (tick-spacing (get tick-spacing pool))
  )
    ;; Price = sqrt(reserve-y / reserve-x) * (1.0001 ^ current-tick)
    (ok (convert-to-uint (* (sqrti (/ reserve-y reserve-x)) (pow 10001 current-tick))))
  )
)

;; Emergency shutdown
(define-public (shutdown-emergency)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set emergency-shutdown true)
    (ok true)
  )
)

;; Recover funds in emergency
(define-public (emergency-withdraw (token-id (string-ascii 20)) (amount uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (not (var-get emergency-shutdown)) err-emergency-shutdown)
    (let (
      (token-info (unwrap! (map-get? token-registry { token-id: token-id }) err-token-not-found))
      (contract-address (get contract token-info))
    )
      (try! (transfer-token token-id amount contract-owner (as-contract contract-address)))
    )
    (ok true)
  )
)

;; Update price oracle
(define-public (update-price-oracle
  (token-id (string-ascii 20))
  (new-oracle principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (let (
      (token-info (unwrap! (map-get? token-registry { token-id: token-id }) err-token-not-found))
    )
      (map-set token-registry
        { token-id: token-id }
        (merge token-info {
          price-oracle: new-oracle,
          last-update-block: block-height
        })
      )
      (map-set oracle-prices
        { token-id: token-id }
        {
          price: (get last-price token-info),
          last-update-block: block-height,
          twap-price: (get last-price token-info),
          trusted: true,
          oracle-address: new-oracle
        }
      )
    )
    (ok true)
  )
)

;; Update volatility
(define-public (update-volatility (token-id (string-ascii 20)) (new-volatility uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (let (
      (token-info (unwrap! (map-get? token-registry { token-id: token-id }) err-token-not-found))
    )
      (map-set token-registry
        { token-id: token-id }
        (merge token-info {
          current-volatility: new-volatility,
          last-update-block: block-height
        })
      )
    )
    (ok true)
  )
)

;; Settle fees and rewards
(define-public (settle-fees-rewards (pool-id uint))
  (begin
    (asserts! (not (var-get emergency-shutdown)) err-emergency-shutdown)
    (let (
      (pool (unwrap! (map-get? liquidity-pools { pool-id: pool-id }) err-pool-not-found))
      (total-fees-x (get total-fees-x pool))
      (total-fees-y (get total-fees-y pool))
      (total-volume-x (get total-volume-x pool))
      (total-volume-y (get total-volume-y pool))
      (protocol-fee-bp (var-get protocol-fee-bp))
      (fee-share-x (/ (* total-fees-x protocol-fee-bp) total-volume-x))
      (fee-share-y (/ (* total-fees-y protocol-fee-bp) total-volume-y))
    )
      ;; Update pool reserves
      (map-set liquidity-pools
        { pool-id: pool-id }
        (merge pool {
          reserve-x: (- (get reserve-x pool) fee-share-x),
          reserve-y: (- (get reserve-y pool) fee-share-y),
          total-fees-x: u0,
          total-fees-y: u0,
          last-update-block: block-height
        })
      )
      (ok true)
    )
  )
)

;; Claim rewards
(define-public (claim-rewards (position-id uint))
  (begin
    (let (
      (position (unwrap! (map-get? liquidity-positions { position-id: position-id }) err-position-not-found))
      (pool (unwrap! (map-get? liquidity-pools { pool-id: (get pool-id position) }) err-pool-not-found))
      (provider (get provider position))
      (il-compensation (get il-compensation position))
      (total-il-compensation-paid (get total-il-compensation-paid pool))
      (new-total-il-compensation-paid (+ total-il-compensation-paid il-compensation))
    )
      (asserts! (not (var-get emergency-shutdown)) err-emergency-shutdown)
      (asserts! (is-eq (get status pool) u0) err-paused) ;; Pool must be active
      (asserts! (>= il-compensation (- (get reserve-x pool) (get total-fees-x pool))) err-insufficient-balance)
      ;; Update position
      (map-set liquidity-positions
        { position-id: position-id }
        (merge position {
          rewards-claimed: (+ (get rewards-claimed position) il-compensation),
          il-compensation: 0,
          last-update-block: block-height
        })
      )
      ;; Update pool
      (map-set liquidity-pools
        { pool-id: (get pool-id position) }
        (merge pool {
          total-il-compensation-paid: new-total-il-compensation-paid,
          last-update-block: block-height
        })
      )
      ;; Transfer compensation
      (try! (transfer-token (get token-x pool) il-compensation provider (as-contract tx-sender)))
      (try! (transfer-token (get token-y pool) il-compensation provider (as-contract tx-sender)))
      (ok true)
    )
  )
)

;; Update protocol parameters
(define-public (update-protocol-parameters
  (new-fee-bp uint)
  (new-min-deposit uint)
  (new-volatility-update-frequency uint)
  (new-max-price-impact-bp uint)
  (new-impermanent-loss-threshold uint)
  (new-impermanent-loss-coverage-bp uint)
  (new-dynamic-range-adjustment-factor uint)
  (new-price-deviation-threshold uint)
  (new-max-dynamic-fee-increase uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-fee-bp u500) err-invalid-parameters) ;; Max 5% base fee
    (asserts! (>= new-min-deposit u1000000) err-min-deposit) ;; Minimum 1 STX
    (asserts! (>= new-volatility-update-frequency u144) err-invalid-parameters) ;; At least once per hour
    (asserts! (<= new-max-price-impact-bp u500) err-invalid-parameters) ;; Max 5% price impact
    (asserts! (<= new-impermanent-loss-threshold u500) err-invalid-parameters) ;; Max 5% IL threshold
    (asserts! (<= new-impermanent-loss-coverage-bp u5000) err-invalid-parameters) ;; Max 50% IL coverage
    (asserts! (<= new-dynamic-range-adjustment-factor u500) err-invalid-parameters) ;; Max 5% range adjustment
    (asserts! (<= new-price-deviation-threshold u200) err-invalid-parameters) ;; Max 2% price deviation
    (asserts! (<= new-max-dynamic-fee-increase u500) err-invalid-parameters) ;; Max 5% fee increase
    ;; Update parameters
    (var-set protocol-fee-bp new-fee-bp)
    (var-set min-deposit-amount new-min-deposit)
    (var-set volatility-update-frequency new-volatility-update-frequency)
    (var-set max-price-impact-bp new-max-price-impact-bp)
    (var-set impermanent-loss-threshold new-impermanent-loss-threshold)
    (var-set impermanent-loss-coverage-bp new-impermanent-loss-coverage-bp)
    (var-set dynamic-range-adjustment-factor new-dynamic-range-adjustment-factor)
    (var-set price-deviation-threshold new-price-deviation-threshold)
    (var-set max-dynamic-fee-increase new-max-dynamic-fee-increase)
    (ok true)
  )
)

;; Pause or unpause the contract
(define-public (set-pause (paused bool))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set emergency-shutdown paused)
    (ok true)
  )
)

;; Update the treasury address
(define-public (update-treasury (new-treasury principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set treasury-address new-treasury)
    (ok true)
  )
)

;; Update the price oracles for all tokens
(define-public (update-all-oracles)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-map
      (lambda (token-id token-info)
        (let (
          (oracle-address (get price-oracle token-info))
          (last-price (get last-price token-info))
        )
          (map-set oracle-prices
            { token-id: token-id }
            {
              price: last-price,
              last-update-block: block-height,
              twap-price: last-price,
              trusted: true,
              oracle-address: oracle-address
            }
          )
        )
      )
      token-registry
    )
    (ok true)
  )
)

;; Admin recovery of tokens
(define-public (admin-recover-token (token-id (string-ascii 20)) (amount uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (let (
      (token-info (unwrap! (map-get? token-registry { token-id: token-id }) err-token-not-found))
      (contract-address (get contract token-info))
    )
      (try! (transfer-token token-id amount contract-owner (as-contract contract-address)))
    )
    (ok true)
  )
)

;; Admin recovery of STX
(define-public (admin-recover-stx (amount uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (try! (stx-transfer? contract-owner amount))
    (ok true)
  )
)

;; View functions

;; Get the current price of a token in STX
(define-public (get-token-price (token-id (string-ascii 20)))
  (let (
    (oracle-data (unwrap! (map-get? oracle-prices { token-id: token-id }) err-token-not-found))
  )
    (ok (get price oracle-data))
  )
)

;; Get the total supply of a token
(define-public (get-token-supply (token-id (string-ascii 20)))
  (let (
    (token-info (unwrap! (map-get? token-registry { token-id: token-id }) err-token-not-found))
  )
    (ok (get max-supply token-info))
  )
)

;; Get the reserves of a liquidity pool
(define-public (get-pool-reserves (pool-id uint))
  (let (
    (pool (unwrap! (map-get? liquidity-pools { pool-id: pool-id }) err-pool-not-found))
  )
    (ok { reserve-x: (get reserve-x pool), reserve-y: (get reserve-y pool) })
  )
)

;; Get the total fees accrued in a pool
(define-public (get-pool-fees (pool-id uint))
  (let (
    (pool (unwrap! (map-get? liquidity-pools { pool-id: pool-id }) err-pool-not-found))
  )
    (ok { total-fees-x: (get total-fees-x pool), total-fees-y: (get total-fees-y pool) })
  )
)

;; Get the current volatility of a token
(define-public (get-token-volatility (token-id (string-ascii 20)))
  (let (
    (token-info (unwrap! (map-get? token-registry { token-id: token-id }) err-token-not-found))
  )
    (ok (get current-volatility token-info))
  )
)

;; Get the price oracle of a token
(define-public (get-token-oracle (token-id (string-ascii 20)))
  (let (
    (token-info (unwrap! (map-get? token-registry { token-id: token-id }) err-token-not-found))
  )
    (ok (get price-oracle token-info))
  )
)

;; Get the status of a liquidity pool
(define-public (get-pool-status (pool-id uint))
  (let (
    (pool (unwrap! (map-get? liquidity-pools { pool-id: pool-id }) err-pool-not-found))
  )
    (ok (get status pool))
  )
)

;; Get the list of active pools
(define-public (get-active-pools)
  (let (
    (all-pools (map-get-all liquidity-pools))
    (active-pools (filter (lambda (pool) (is-eq (get status pool) u0)) (map-get-all liquidity-pools)))
  )
    (ok (map (lambda (pool) { pool-id: (get pool-id pool), token-x: (get token-x pool), token-y: (get token-y pool), reserve-x: (get reserve-x pool), reserve-y: (get reserve-y pool), liquidity-units: (get liquidity-units pool), curve-type: (get curve-type pool), base-fee-bp: (get base-fee-bp pool), dynamic-fee-bp: (get dynamic-fee-bp pool), current-tick: (get current-tick pool), tick-spacing: (get tick-spacing pool), price-oracle: (get price-oracle pool), total-volume-x: (get total-volume-x pool), total-volume-y: (get total-volume-y pool), total-fees-x: (get total-fees-x pool), total-fees-y: (get total-fees-y pool), total-fees-protocol: (get total-fees-protocol pool), creation-block: (get creation-block pool), last-update-block: (get last-update-block pool), status: (get status pool), price-history: (get price-history pool), volatility-adjustment: (get volatility-adjustment pool), concentrated-ranges: (get concentrated-ranges pool), total-il-compensation-paid: (get total-il-compensation-paid pool) }