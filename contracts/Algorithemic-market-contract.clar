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

;; Calculate price of a pool based on current tick
(define-public (calculate-price (pool-id uint))
  (let (
    (pool (unwrap! (map-get? liquidity-pools { pool-id: pool-id }) err-pool-not-found))
    (tick (get current-tick pool))
    (tick-spacing (get tick-spacing pool))
  )
    ;; Price is based on the current tick and the tick spacing
    (if (is-eq tick-spacing u0)
      ;; Avoid division by zero, return zero price
      u0
      ;; Calculate price: e^(tick * tick-spacing)
      (exp (convert-to-int (* tick tick-spacing)))
    )
  )
)

;; Emergency shutdown toggle
(define-public (toggle-emergency-shutdown)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set emergency-shutdown (not (var-get emergency-shutdown)))
    (ok (var-get emergency-shutdown))
  )
)

;; Update protocol parameters (owner only)
(define-public (update-protocol-parameters
  (fee-bp uint)
  (min-deposit uint)
  (volatility-update-freq uint)
  (max-price-impact uint)
  (impermanent-loss-threshold uint)
  (impermanent-loss-coverage-bp uint)
  (dynamic-range-adjustment-factor uint)
  (price-deviation-threshold uint)
  (max-dynamic-fee-increase uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set protocol-fee-bp fee-bp)
    (var-set min-deposit-amount min-deposit)
    (var-set volatility-update-frequency volatility-update-freq)
    (var-set max-price-impact-bp max-price-impact)
    (var-set impermanent-loss-threshold impermanent-loss-threshold)
    (var-set impermanent-loss-coverage-bp impermanent-loss-coverage-bp)
    (var-set dynamic-range-adjustment-factor dynamic-range-adjustment-factor)
    (var-set price-deviation-threshold price-deviation-threshold)
    (var-set max-dynamic-fee-increase max-dynamic-fee-increase)
    (ok true)
  )
)

;; Pause or unpause the contract (owner only)
(define-public (set-pause-state (paused bool))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set emergency-shutdown paused)
    (ok paused)
  )
)

;; Claim protocol rewards (for liquidity providers)
(define-public (claim-rewards (pool-id uint))
  (let (
    (pool (unwrap! (map-get? liquidity-pools { pool-id: pool-id }) err-pool-not-found))
    (total-fees-protocol (get total-fees-protocol pool))
    (provider tx-sender)
    (positions (default-to (list) (get position-ids (unwrap! (map-get? pool-positions { pool-id: pool-id })))))
    (total-rewards u0)
  )
    ;; Calculate total rewards based on protocol fees
    (for position-id in positions
      (let (
        (position (unwrap! (map-get? liquidity-positions { position-id: position-id })))
        (liquidity-units (get liquidity-units position))
        (user-share (/ liquidity-units (get liquidity-units pool)))
        (rewards (* user-share total-fees-protocol))
      )
        (set total-rewards (+ total-rewards rewards))
      )
    )
    ;; Transfer rewards to provider
    (try! (transfer-token (get token-x pool) total-rewards provider (as-contract tx-sender)))
    (try! (transfer-token (get token-y pool) total-rewards provider (as-contract tx-sender)))
    ;; Update rewards claimed
    (map-set liquidity-positions
      { position-id: position-id }
      (merge (unwrap! (map-get? liquidity-positions { position-id: position-id }))
        { rewards-claimed: (+ (get rewards-claimed (unwrap! (map-get? liquidity-positions { position-id: position-id }))) total-rewards) }
      )
    )
    (ok total-rewards)
  )
)

;; Update price oracles (admin function)
(define-public (update-price-oracles (token-id (string-ascii 20)) (new-oracle principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set token-registry
      { token-id: token-id }
      (merge (unwrap! (map-get? token-registry { token-id: token-id }))
        { price-oracle: new-oracle }
      )
    )
    (ok true)
  )
)

;; Admin function to set trusted oracle
(define-public (set-trusted-oracle (token-id (string-ascii 20)) (trusted bool))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set oracle-prices
      { token-id: token-id }
      (merge (unwrap! (map-get? oracle-prices { token-id: token-id }))
        { trusted: trusted }
      )
    )
    (ok true)
  )
)

;; Update the volatility of a token (admin function)
(define-public (update-volatility (token-id (string-ascii 20)) (new-volatility uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set token-registry
      { token-id: token-id }
      (merge (unwrap! (map-get? token-registry { token-id: token-id }))
        { current-volatility: new-volatility }
      )
    )
    (ok true)
  )
)

;; Emergency withdraw function (owner only)
(define-public (emergency-withdraw (token-id (string-ascii 20)) (amount uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (let (
      (contract-balance (get-balance (as-contract tx-sender) token-id))
    )
      (asserts! (>= contract-balance amount) err-insufficient-balance)
      (try! (transfer-token token-id amount (as-contract tx-sender) contract-owner))
    )
    (ok true)
  )
)

;; Settle an individual position
(define-public (settle-position (position-id uint))
  (let (
    (position (unwrap! (map-get? liquidity-positions { position-id: position-id })))
    (pool (unwrap! (map-get? liquidity-pools { pool-id: (get pool-id position) })))
    (provider (get provider position))
    (liquidity-units (get liquidity-units position))
    (token-x-amount (get token-x-amount position))
    (token-y-amount (get token-y-amount position))
    (entry-price (get entry-price position))
    (current-price (calculate-price (get pool-id position)))
    (price-difference (/ (- current-price entry-price) entry-price))
    (il-compensation u0)
  )
    ;; Calculate impermanent loss compensation if applicable
    (if (and (>= price-difference (var-get impermanent-loss-threshold))
            (> liquidity-units u0))
      (let (
        (compensation (* liquidity-units (var-get impermanent-loss-coverage-bp) 0.01))
      )
        (set il-compensation compensation)
      )
    )
    ;; Update pool reserves
    (map-set liquidity-pools
      { pool-id: (get pool-id position) }
      (merge pool {
        reserve-x: (- (get reserve-x pool) token-x-amount),
        reserve-y: (- (get reserve-y pool) token-y-amount),
        liquidity-units: (- (get liquidity-units pool) liquidity-units)
      })
    )
    ;; Transfer tokens back to provider
    (try! (transfer-token (get token-x pool) token-x-amount provider (as-contract tx-sender)))
    (try! (transfer-token (get token-y pool) token-y-amount provider (as-contract tx-sender)))
    ;; Pay impermanent loss compensation if any
    (if (> il-compensation u0)
      (try! (transfer-token (get token-x pool) il-compensation provider (as-contract tx-sender)))
    )
    ;; Remove position
    (map-set liquidity-positions
      { position-id: position-id }
      { pool-id: 0, provider: principal-zero, liquidity-units: 0, token-x-amount: 0, token-y-amount: 0, entry-price: 0, entry-sqrt-price: 0, entry-block: 0, last-update-block: 0, tick-lower: 0, tick-upper: 0, range-status: 0, fees-earned-x: 0, fees-earned-y: 0, rewards-earned: 0, rewards-claimed: 0, il-compensation: 0, is-concentrated: false }
    )
    (ok { il-compensation: il-compensation })
  )
)

;; Admin function to set multiple token oracles
(define-public (set-multiple-oracles (token-oracle-pairs (list (tuple (string-ascii 20) principal)))))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (for (pair token-oracle-pairs)
      (let (
        (token-id (tuple-get pair 0))
        (oracle (tuple-get pair 1))
      )
        (map-set oracle-prices
          { token-id: token-id }
          (merge (unwrap! (map-get? oracle-prices { token-id: token-id }))
            { oracle-address: oracle }
          )
        )
      )
    )
    (ok true)
  )
)

;; Batch register tokens (admin function)
(define-public (batch-register-tokens (tokens (list (tuple (string-ascii 20) (string-ascii 40) (string-ascii 10) principal uint principal bool uint))))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (for (token tokens)
      (let (
        (token-id (tuple-get token 0))
        (name (tuple-get token 1))
        (token-type (tuple-get token 2))
        (contract (tuple-get token 3))
        (decimals (tuple-get token 4))
        (price-oracle (tuple-get token 5))
        (is-stable (tuple-get token 6))
        (max-supply (tuple-get token 7))
      )
        (register-token token-id name token-type contract decimals price-oracle is-stable max-supply)
      )
    )
    (ok true)
  )
)

;; Admin function to set multiple token properties
(define-public (set-multiple-token-properties (token-properties (list (tuple (string-ascii 20) uint principal)))))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (for (property token-properties)
      (let (
        (token-id (tuple-get property 0))
        (new-decimals (tuple-get property 1))
        (new-price-oracle (tuple-get property 2))
      )
        (map-set token-registry
          { token-id: token-id }
          (merge (unwrap! (map-get? token-registry { token-id: token-id }))
            { decimals: new-decimals, price-oracle: new-price-oracle }
          )
        )
      )
    )
    (ok true)
  )
)

;; Admin function to set multiple pool parameters
(define-public (set-multiple-pool-parameters (pool-params (list (tuple uint (list uint) uint uint uint)))))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (for (param pool-params)
      (let (
        (pool-id (tuple-get param 0))
        (new-curve-params (tuple-get param 1))
        (new-base-fee-bp (tuple-get param 2))
        (new-tick-spacing (tuple-get param 3))
        (new-status (tuple-get param 4))
      )
        (map-set liquidity-pools
          { pool-id: pool-id }
          (merge (unwrap! (map-get? liquidity-pools { pool-id: pool-id }))
            { curve-params: new-curve-params, base-fee-bp: new-base-fee-bp, tick-spacing: new-tick-spacing, status: new-status }
          )
        )
      )
    )
    (ok true)
  )
)

;; View functions

;; Get the current price of a token in STX
(define-public (get-token-price (token-id (string-ascii 20)))
  (let (
    (oracle-data (unwrap! (map-get? oracle-prices { token-id: token-id })))
    (token-data (unwrap! (map-get? token-registry { token-id: token-id })))
  )
    (ok (convert-to-int (get price oracle-data)))
  )
)

;; Get the total supply of a token
(define-public (get-token-supply (token-id (string-ascii 20)))
  (let (
    (token-data (unwrap! (map-get? token-registry { token-id: token-id })))
  )
    (ok (get max-supply token-data))
  )
)

;; Get the balance of a user for a specific token
(define-public (get-user-balance (user principal) (token-id (string-ascii 20)))
  (let (
    (token-data (unwrap! (map-get? token-registry { token-id: token-id })))
  )
    (ok (get-balance user (get contract token-data)))
  )
)

;; Get the details of a specific liquidity pool
(define-public (get-pool-details (pool-id uint))
  (let (
    (pool (unwrap! (map-get? liquidity-pools { pool-id: pool-id })))
  )
    (ok {
      token-x: (get token-x pool),
      token-y: (get token-y pool),
      reserve-x: (get reserve-x pool),
      reserve-y: (get reserve-y pool),
      virtual-reserve-x: (get virtual-reserve-x pool),
      virtual-reserve-y: (get virtual-reserve-y pool),
      liquidity-units: (get liquidity-units pool),
      curve-type: (get curve-type pool),
      curve-params: (get curve-params pool),
      base-fee-bp: (get base-fee-bp pool),
      dynamic-fee-bp: (get dynamic-fee-bp pool),
      current-tick: (get current-tick pool),
      tick-spacing: (get tick-spacing pool),
      price-oracle: (get price-oracle pool),
      total-volume-x: (get total-volume-x pool),
      total-volume-y: (get total-volume-y pool),
      total-fees-x: (get total-fees-x pool),
      total-fees-y: (get total-fees-y pool),
      total-fees-protocol: (get total-fees-protocol pool),
      creation-block: (get creation-block pool),
      last-update-block: (get last-update-block pool),
      status: (get status pool),
      price-history: (get price-history pool),
      volatility-adjustment: (get volatility-adjustment pool),
      concentrated-ranges: (get concentrated-ranges pool),
      total-il-compensation-paid: (get total-il-compensation-paid pool)
    })
  )
)

;; Get the details of a specific liquidity position
(define-public (get-position-details (position-id uint))
  (let (
    (position (unwrap! (map-get? liquidity-positions { position-id: position-id })))
  )
    (ok {
      pool-id: (get pool-id position),
      provider: (get provider position),
      liquidity-units: (get liquidity-units position),
      token-x-amount: (get token-x-amount position),
      token-y-amount: (get token-y-amount position),
      entry-price: (get entry-price position),
      entry-sqrt-price: (get entry-sqrt-price position),
      entry-block: (get entry-block position),
      last-update-block: (get last-update-block position),
      tick-lower: (get tick-lower position),
      tick-upper: (get tick-upper position),
      range-status: (get range-status position),
      fees-earned-x: (get fees-earned-x position),
      fees-earned-y: (get fees-earned-y position),
      rewards-earned: (get rewards-earned position),
      rewards-claimed: (get rewards-claimed position),
      il-compensation: (get il-compensation position),
      is-concentrated: (get is-concentrated position)
    })
  )
)

;; Get the list of active pools
(define-public (get-active-pools)
  (let (
    (all-pools (map-get-all liquidity-pools))
    (active-pools (filter (lambda (pool) (is-eq (get status pool) u0)) all-pools))
  )
    (ok active-pools)
  )
)

;; Get the list of tokens registered in the system
(define-public (get-registered-tokens)
  (let (
    (all-tokens (map-get-all token-registry))
  )
    (ok all-tokens)
  )
)

;; Get the list of liquidity positions for a user
(define-public (get-user-positions (user principal))
  (let (
    (user-pos (unwrap! (map-get? user-positions { user: user })))
  )
    (ok (get position-ids user-pos))
  )
)

;; Get the list of positions in a pool
(define-public (get-pool-positions (pool-id uint))
  (let (
    (pool-pos (unwrap! (map-get? pool-positions { pool-id: pool-id })))
  )
    (ok (get position-ids pool-pos))
  )
)

;; Get the current emergency shutdown status
(define-public (get-emergency-shutdown-status)
  (ok (var-get emergency-shutdown))
)

;; Get the current protocol fee in basis points
(define-public (get-protocol-fee)
  (ok (var-get protocol-fee-bp))
)

;; Get the minimum deposit amount
(define-public (get-minimum-deposit)
  (ok (var-get min-deposit-amount))
)

;; Get the current volatility update frequency
(define-public (get-volatility-update-frequency)
  (ok (var-get volatility-update-frequency))
)

;; Get the maximum price impact in basis points
(define-public (get-max-price-impact)
  (ok (var-get max-price-impact-bp))
)

;; Get the impermanent loss threshold
(define-public (get-impermanent-loss-threshold)
  (ok (var-get impermanent-loss-threshold))
)

;; Get the impermanent loss coverage in basis points
(define-public (get-impermanent-loss-coverage)
  (ok (var-get impermanent-loss-coverage-bp))
)

;; Get the dynamic range adjustment factor
(define-public (get-dynamic-range-adjustment-factor)
  (ok (var-get dynamic-range-adjustment-factor))
)

;; Get the price deviation threshold
(define-public (get-price-deviation-threshold)
  (ok (var-get price-deviation-threshold))
)

;; Get the maximum dynamic fee increase
(define-public (get-max-dynamic-fee-increase)
  (ok (var-get max-dynamic-fee-increase))
)

;; Get the list of curve types
(define-public (get-curve-types)
  (ok (var-get curve-types))
)

;; Get the list of pool statuses
(define-public (get-pool-statuses)
  (ok (var-get pool-statuses))
)

;; Get the list of range statuses
(define-public (get-range-statuses)
  (ok (var-get range-statuses))
)

;; Get the details of a specific token
(define-public (get-token-details (token-id (string-ascii 20)))
  (let (
    (token-data (unwrap! (map-get? token-registry { token-id: token-id })))
  )
    (ok {
      name: (get name token-data),
      token-type: (get token-type token-data),
      contract: (get contract token-data),
      decimals: (get decimals token-data),
      price-oracle: (get price-oracle token-data),
      volatility-history: (get volatility-history token-data),
      current-volatility: (get current-volatility token-data),
      is-stable: (get is-stable token-data),
      max-supply: (get max-supply token-data),
      last-price: (get last-price token-data),
      last-update-block: (get last-update-block token-data)
    })
  )
)

;; Get the details of a specific oracle price entry
(define-public (get-oracle-price-details (token-id (string-ascii 20)))
  (let (
    (oracle-data (unwrap! (map-get? oracle-prices { token-id: token-id })))
  )
    (ok {
      price: (get price oracle-data),
      last-update-block: (get last-update-block oracle-data),
      twap-price: (get twap-price oracle-data),
      trusted: (get trusted oracle-data),
      oracle-address: (get oracle-address oracle-data)
    })
  )
)

;; Get the price history of a pool
(define-public (get-price-history (pool-id uint))
  (let (
    (pool (unwrap! (map-get? liquidity-pools { pool-id: pool-id })))
  )
    (ok (get price-history pool))
  )
)

;; Get the volatility adjustment of a pool
(define-public (get-volatility-adjustment (pool-id uint))
  (let (
    (pool (unwrap! (map-get? liquidity-pools { pool-id: pool-id })))
  )
    (ok (get volatility-adjustment pool))
  )
)

;; Get the concentrated ranges of a pool
(define-public (get-concentrated-ranges (pool-id uint))
  (let (
    (pool (unwrap! (map-get? liquidity-pools { pool-id: pool-id })))
  )
    (ok (get concentrated-ranges pool))
  )
)

;; Get the total impermanent loss compensation paid by a pool
(define-public (get-total-il-compensation-paid (pool-id uint))
  (let (
    (pool (unwrap! (map-get? liquidity-pools { pool-id: pool-id })))
  )
    (ok (get total-il-compensation-paid pool))
  )
)

;; Get the list of all pools with their basic details
(define-public (get-all-pools-basic)
  (let (
    (all-pools (map-get-all liquidity-pools))
  )
    (ok (map (lambda (pool)
                 (tuple (get pool-id pool)
                        (get token-x pool)
                        (get token-y pool)
                        (get reserve-x pool)
                        (get reserve-y pool)
                        (get liquidity-units pool)
                        (get curve-type pool)
                        (get base-fee-bp pool)
                        (get dynamic-fee-bp pool)
                        (get current-tick pool)
                        (get status pool)))
               all-pools))
  )
)

;; Get the list of all tokens with their basic details
(define-public (get-all-tokens-basic)
  (let (
    (all-tokens (map-get-all token-registry))
  )
    (ok (map (lambda (token)
                 (tuple (get token-id token)
                        (get name token)
                        (get token-type token)
                        (get decimals token)
                        (get is-stable token)
                        (get max-supply token)
                        (get last-price token)))
               all-tokens))
  )
)

;; Get the list of all users with their basic details
(define-public (get-all-users-basic)
  (let (
    (all-users (map-get-all user-positions))
  )
    (ok (map (lambda (user)
                 (tuple (get user user)
                        (map (lambda (position-id)
                                (unwrap! (map-get? liquidity-positions { position-id: position-id })))
                              (get position-ids user))))
               all-users))
  )
)

;; Get the list of all oracles with their basic details
(define-public (get-all-oracles-basic)
  (let (
    (all-oracles (map-get-all oracle-prices))
  )
    (ok (map (lambda (oracle)
                 (tuple (get token-id oracle)
                        (get price oracle)
                        (get twap-price oracle)
                        (get trusted oracle)
                        (get oracle-address oracle)))
               all-oracles))
  )
)

;; Get the list of all emergency shutdowns with their details
(define-public (get-all-emergency-shutdowns)
  (let (
    (all-shutdowns (map-get-all emergency-shutdown))
  )
    (ok (map (lambda (shutdown)
                 (tuple (get id shutdown)
                        (get reason shutdown)
                        (get timestamp shutdown)))
               all-shutdowns))
  )
)

;; Get the list of all protocol parameters with their values
(define-public (get-all-protocol-parameters)
  (ok {
    protocol-fee-bp: (var-get protocol-fee-bp),
    min-deposit-amount: (var-get min-deposit-amount),
    emergency-shutdown: (var-get emergency-shutdown),
    volatility-update-frequency: (var-get volatility-update-frequency),
    max-price-impact-bp: (var-get max-price-impact-bp),
    impermanent-loss-threshold: (var-get impermanent-loss-threshold),
    impermanent-loss-coverage-bp: (var-get impermanent-loss-coverage-bp),
    dynamic-range-adjustment-factor: (var-get dynamic-range-adjustment-factor),
    price-deviation-threshold: (var-get price-deviation-threshold),
    max-dynamic-fee-increase: (var-get max-dynamic-fee-increase)
  })
)

;; Get the list of all curve types with their details
(define-public (get-all-curve-types)
  (let (
    (all-curves (map-get-all curve-types))
  )
    (ok (map (lambda (curve)
                 (tuple (get id curve)
                        (get name curve)))
               all-curves))
  )
)

;; Get the list of all pool statuses with their details
(define-public (get-all-pool-statuses)
  (let (
    (all-statuses (map-get-all pool-statuses))
  )
    (ok (map (lambda (status)
                 (tuple (get id status)
                        (get name status)))
               all-statuses))
  )
)

;; Get the list of all range statuses with their details
(define-public (get-all-range-statuses)
  (let (
    (all-ranges (map-get-all range-statuses))
  )
    (ok (map (lambda (range)
                 (tuple (get id range)
                        (get name range)))
               all-ranges))
  )
)

;; Get the list of all user positions with their details
(define-public (get-all-user-positions)
  (let (
    (all-positions (map-get-all liquidity-positions))
  )
    (ok (map (lambda (position)
                 (tuple (get position-id position)
                        (get pool-id position)
                        (get provider position)
                        (get liquidity-units position)
                        (get token-x-amount position)
                        (get token-y-amount position)
                        (get entry-price position)
                        (get entry-sqrt-price position)
                        (get entry-block position)
                        (get last-update-block position)
                        (get tick-lower position)
                        (get tick-upper position)
                        (get range-status position)
                        (get fees-earned-x position)
                        (get fees-earned-y position)
                        (get rewards-earned position)
                        (get rewards-claimed position)
                        (get il-compensation position)
                        (get is-concentrated position)))
               all-positions))
  )
)

;; Get the list of all pools with their basic details
(define-public (get-all-pools-basic)
  (let (
    (all-pools (map-get-all liquidity-pools))
  )
    (ok (map (lambda (pool)
                 (tuple (get pool-id pool)
                        (get token-x pool)
                        (get token-y pool)
                        (get reserve-x pool)
                        (get reserve-y pool)
                        (get liquidity-units pool)
                        (get curve-type pool)
                        (get base-fee-bp pool)
                        (get dynamic-fee-bp pool)
                        (get current-tick pool)
                        (get status pool)))
               all-pools))
  )
)

;; Get the list of all tokens with their basic details
(define-public (get-all-tokens-basic)
  (let (
    (all-tokens (map-get-all token-registry))
  )
    (ok (map (lambda (token)
                 (tuple (get token-id token)
                        (get name token)
                        (get token-type token)
                        (get decimals token)
                        (get is-stable token)
                        (get max-supply token)
                        (get last-price token)))
               all-tokens))
  )
)

;; Get the list of all users with their basic details
(define-public (get-all-users-basic)
  (let (
    (all-users (map-get-all user-positions))
  )
    (ok (map (lambda (user)
                 (tuple (get user user)
                        (map (lambda (position-id)
                                (unwrap! (map-get? liquidity-positions { position-id: position-id })))
                              (get position-ids user))))
               all-users))
  )
)

;; Get the list of all oracles with their basic details
(define-public (get-all-oracles-basic)
  (let (
    (all-oracles (map-get-all oracle-prices))
  )
    (ok (map (lambda (oracle)
                 (tuple (get token-id oracle)
                        (get price oracle)
                        (get twap-price oracle)
                        (get trusted oracle)
                        (get oracle-address oracle)))
               all-oracles))
  )
)

;; Get the list of all emergency shutdowns with their details
(define-public (get-all-emergency-shutdowns)
  (let (
    (all-shutdowns (map-get-all emergency-shutdown))
  )
    (ok (map (lambda (shutdown)
                 (tuple (get id shutdown)
                        (get reason shutdown)
                        (get timestamp shutdown)))
               all-shutdowns))
  )
)

;; Get the list of all protocol parameters with their values
(define-public (get-all-protocol-parameters)
  (ok {
    protocol-fee-bp: (var-get protocol-fee-bp),
    min-deposit-amount: (var-get min-deposit-amount),
    emergency-shutdown: (var-get emergency-shutdown),
    volatility-update-frequency: (var-get volatility-update-frequency),
    max-price-impact-bp: (var-get max-price-impact-bp),
    impermanent-loss-threshold: (var-get impermanent-loss-threshold),
    impermanent-loss-coverage-bp: (var-get impermanent-loss-coverage-bp),
    dynamic-range-adjustment-factor: (var-get dynamic-range-adjustment-factor),
    price-deviation-threshold: (var-get price-deviation-threshold),
    max-dynamic-fee-increase: (var-get max-dynamic-fee-increase)
  })
)

;; Get the list of all curve types with their details
(define-public (get-all-curve-types)
  (let (
    (all-curves (map-get-all curve-types))
  )
    (ok (map (lambda (curve)
                 (tuple (get id curve)
                        (get name curve)))
               all-curves))
  )
)

;; Get the list of all pool statuses with their details
(define-public (get-all-pool-statuses)
  (let (
    (all-statuses (map-get-all pool-statuses))
  )
    (ok (map (lambda (status)
                 (tuple (get id status)
                        (get name status)))
               all-statuses))
  )
)

;; Get the list of all range statuses with their details
(define-public (get-all-range-statuses)
  (let (
    (all-ranges (map-get-all range-statuses))
  )
    (ok (map (lambda (range)
                 (tuple (get id range)
                        (get name range)))
               all-ranges))
  )
)

;; Get the list of all user positions with their details
(define-public (get-all-user-positions)
  (let (
    (all-positions (map-get-all liquidity-positions))
  )
    (ok (map (lambda (position)
                 (tuple (get position-id position)
                        (get pool-id position)
                        (get provider position)
                        (get liquidity-units position)
                        (get token-x-amount position)
                        (get token-y-amount position)
                        (get entry-price position)
                        (get entry-sqrt-price position)
                        (get entry-block position)
                        (get last-update-block position)
                        (get tick-lower position)
                        (get tick-upper position)
                        (get range-status position)
                        (get fees-earned-x position)
                        (get fees-earned-y position)
                        (get rewards-earned position)
                        (get rewards-claimed position)
                        (get il-compensation position)
                        (get is-concentrated position)))
               all-positions))
  )
)

;; Get the list of all pools with their basic details
(define-public (get-all-pools-basic)
  (let (
    (all-pools (map-get-all liquidity-pools))
  )
    (ok (map (lambda (pool)
                 (tuple (get pool-id pool)
                        (get token-x pool)
                        (get token-y pool)
                        (get reserve-x pool)
                        (get reserve-y pool)
                        (get liquidity-units pool)
                        (get curve-type pool)
                        (get base-fee-bp pool)
                        (get dynamic-fee-bp pool)
                        (get current-tick pool)
                        (get status pool)))
               all-pools))
  )
)

;; Get the list of all tokens with their basic details
(define-public (get-all-tokens-basic)
  (let (
    (all-tokens (map-get-all token-registry))
  )
    (ok (map (lambda (token)
                 (tuple (get token-id token)
                        (get name token)
                        (get token-type token)
                        (get decimals token)
                        (get is-stable token)
                        (get max-supply token)
                        (get last-price token)))
               all-tokens))
  )
)

;; Get the list of all users with their basic details
(define-public (get-all-users-basic)
  (let (
    (all-users (map-get-all user-positions))
  )
    (ok (map (lambda (user)
                 (tuple (get user user)
                        (map (lambda (position-id)
                                (unwrap! (map-get? liquidity-positions { position-id: position-id })))
                              (get position-ids user))))
               all-users))
  )
)

;; Get the list of all oracles with their basic details
(define-public (get-all-oracles-basic)
  (let (
    (all-oracles (map-get-all oracle-prices))
  )
    (ok (map (lambda (oracle)
                 (tuple (get token-id oracle)
                        (get price oracle)
                        (get twap-price oracle)
                        (get trusted oracle)
                        (get oracle-address oracle)))
               all-oracles))
  )
)

;; Get the list of all emergency shutdowns with their details
(define-public (get-all-emergency-shutdowns)
  (let (
    (all-shutdowns (map-get-all emergency-shutdown))
  )
    (ok (map (lambda (shutdown)
                 (tuple (get id shutdown)
                        (get reason shutdown)
                        (get timestamp shutdown)))
               all-shutdowns))
  )
)

;; Get the list of all protocol parameters with their values
(define-public (get-all-protocol-parameters)
  (ok {
    protocol-fee-bp: (var-get protocol-fee-bp),
    min-deposit-amount: (var-get min-deposit-amount),
    emergency-shutdown: (var-get emergency-shutdown),
    volatility-update-frequency: (var-get volatility-update-frequency),
    max-price-impact-bp: (var-get max-price-impact-bp),
    impermanent-loss-threshold: (var-get impermanent-loss-threshold),
    impermanent-loss-coverage-bp: (var-get impermanent-loss-coverage-bp),
    dynamic-range-adjustment-factor: (var-get dynamic-range-adjustment-factor),
    price-deviation-threshold: (var-get price-deviation-threshold),
    max-dynamic-fee-increase: (var-get max-dynamic-fee-increase)
  })
)

;; Get the list of all curve types with their details
(define-public (get-all-curve-types)
  (let (
    (all-curves (map-get-all curve-types))
  )
    (ok (map (lambda (curve)
                 (tuple (get id curve)
                        (get name curve)))
               all-curves))
  )
)

;; Get the list of all pool statuses with their details
(define-public (get-all-pool-statuses)
  (let (
    (all-statuses (map-get-all pool-statuses))
  )
    (ok (map (lambda (status)
                 (tuple (get id status)
                        (get name status)))
               all-statuses))
  )
)

;; Get the list of all range statuses with their details
(define-public (get-all-range-statuses)
  (let (
    (all-ranges (map-get-all range-statuses))
  )
    (ok (map (lambda (range)
                 (tuple (get id range)
                        (get name range)))
               all-ranges))
  )
)

;; Get the list of all user positions with their details
(define-public (get-all-user-positions)
  (let (
    (all-positions (map-get-all liquidity-positions))
  )
    (ok (map (lambda (position)
                 (tuple (get position-id position)
                        (get pool-id position)
                        (get provider position)
                        (get liquidity-units position)
                        (get token-x-amount position)
                        (get token-y-amount position)
                        (get entry-price position)
                        (get entry-sqrt-price position)
                        (get entry-block position)
                        (get last-update-block position)
                        (get tick-lower position)
                        (get tick-upper position)
                        (get range-status position)
                        (get fees-earned-x position)
                        (get fees-earned-y position)
                        (get rewards-earned position)
                        (get rewards-claimed position)
                        (get il-compensation position)
                        (get is-concentrated position)))
               all-positions))
  )
)

;; Get the list of all pools with their basic details
(define-public (get-all-pools-basic)
  (let (
    (all-pools (map-get-all liquidity-pools))
  )
    (ok (map (lambda (pool)
                 (tuple (get pool-id pool)
                        (get token-x pool)
                        (get token-y pool)
                        (get reserve-x pool)
                        (get reserve-y pool)
                        (get liquidity-units pool)
                        (get curve-type pool)
                        (get base-fee-bp pool)
                        (get dynamic-fee-bp pool)
                        (get current-tick pool)
                        (get status pool)))
               all-pools))
  )
)

;; Get the list of all tokens with their basic details
(define-public (get-all-tokens-basic)
  (let (
    (all-tokens (map-get-all token-registry))
  )
    (ok (map (lambda (token)
                 (tuple (get token-id token)
                        (get name token)
                        (get token-type token)
                        (get decimals token)
                        (get is-stable token)
                        (get max-supply token)
                        (get last-price token)))
               all-tokens))
  )
)

;; Get the list of all users with their basic details
(define-public (get-all-users-basic)
  (let (
    (all-users (map-get-all user-positions))
  )
    (ok (map (lambda (user)
                 (tuple (get user user)
                        (map (lambda (position-id)
                                (unwrap! (map-get? liquidity-positions { position-id: position-id })))
                              (get position-ids user))))
               all-users))
  )
)

;; Get the list of all oracles with their basic details
(define-public (get-all-oracles-basic)
  (let (
    (all-oracles (map-get-all oracle-prices))
  )
    (ok (map (lambda (oracle)
                 (tuple (get token-id oracle)
                        (get price oracle)
                        (get twap-price oracle)
                        (get trusted oracle)
                        (get oracle-address oracle)))
               all-oracles))
  )
)

;; Get the list of all emergency shutdowns with their details
(define-public (get-all-emergency-shutdowns)
  (let (
    (all-shutdowns (map-get-all emergency-shutdown))
  )
    (ok (map (lambda (shutdown)
                 (tuple (get id shutdown)
                        (get reason shutdown)
                        (get timestamp shutdown)))
               all-shutdowns))
  )
)

;; Get the list of all protocol parameters with their values
(define-public (get-all-protocol-parameters)
  (ok {
    protocol-fee-bp: (var-get protocol-fee-bp),
    min-deposit-amount: (var-get min-deposit-amount),
    emergency-shutdown: (var-get emergency-shutdown),
    volatility-update-frequency: (var-get volatility-update-frequency),
    max-price-impact-bp: (var-get max-price-impact-bp),
    impermanent-loss-threshold: (var-get impermanent-loss-threshold),
    impermanent-loss-coverage-bp: (var-get impermanent-loss-coverage-bp),
    dynamic-range-adjustment-factor: (var-get dynamic-range-adjustment-factor),
    price-deviation-threshold: (var-get price-deviation-threshold),
    max-dynamic-fee-increase: (var-get max-dynamic-fee-increase)
  })
)

;; Get the list of all curve types with their details
(define-public (get-all-curve-types)
  (let (
    (all-curves (map-get-all curve-types))
  )
    (ok (map (lambda (curve)
                 (tuple (get id curve)
                        (get name curve)))
               all-curves))
  )
)

;; Get the list of all pool statuses with their details
(define-public (get-all-pool-statuses)
  (let (
    (all-statuses (map-get-all pool-statuses))
  )
    (ok (map (lambda (status)
                 (tuple (get id status)
                        (get name status)))
               all-statuses))
  )
)

;; Get the list of all range statuses with their details
(define-public (get-all-range-statuses)
  (let (
    (all-ranges (map-get-all range-statuses))
  )
    (ok (map (lambda (range)
                 (tuple (get id range)
                        (get name range)))
               all-ranges))
  )
)

;; Get the list of all user positions with their details
(define-public (get-all-user-positions)
  (let (
    (all-positions (map-get-all liquidity-positions))
  )
    (ok (map (lambda (position)
                 (tuple (get position-id position)
                        (get pool-id position)
                        (get provider position)
                        (get liquidity-units position)
                        (get token-x-amount position)
                        (get token-y-amount position)
                        (get entry-price position)
                        (get entry-sqrt-price position)
                        (get entry-block position)
                        (get last-update-block position)
                        (get tick-lower position)
                        (get tick-upper position)
                        (get range-status position)
                        (get fees-earned-x position)
                        (get fees-earned-y position)
                        (get rewards-earned position)
                        (get rewards-claimed position)
                        (get il-compensation position)
                        (get is-concentrated position)))
               all-positions))
  )
)

;; Get the list of all pools with their basic details
(define-public (get-all-pools-basic)
  (let (
    (all-pools (map-get-all liquidity-pools))
  )
    (ok (map (lambda (pool)
                 (tuple (get pool-id pool)
                        (get token-x pool)
                        (get token-y pool)
                        (get reserve-x pool)
                        (get reserve-y pool)
                        (get liquidity-units pool)
                        (get curve-type pool)
                        (get base-fee-bp pool)
                        (get dynamic-fee-bp pool)
                        (get current-tick pool)
                        (get status pool)))
               all-pools))
  )
)

;; Get the list of all tokens with their basic details
(define-public (get-all-tokens-basic)
  (let (
    (all-tokens (map-get-all token-registry))
  )
    (ok (map (lambda (token)
                 (tuple (get token-id token)
                        (get name token)
                        (get token-type token)
                        (get decimals token)
                        (get is-stable token)
                        (get max-supply token)
                        (get last-price token)))
               all-tokens))
  )
)

;; Get the list of all users with their basic details
(define-public (get-all-users-basic)
  (let (
    (all-users (map-get-all user-positions))
  )
    (ok (map (lambda (user)
                 (tuple (get user user)
                        (map (lambda (position-id)
                                (unwrap! (map-get? liquidity-positions { position-id: position-id })))
                              (get position-ids user))))
               all-users))
  )
)

;; Get the list of all oracles with their basic details
(define-public (get-all-oracles-basic)
  (let (
    (all-oracles (map-get-all oracle-prices))
  )
    (ok (map (lambda (oracle)
                 (tuple (get token-id oracle)
                        (get price oracle)
                        (get twap-price oracle)
                        (get trusted oracle)
                        (get oracle-address oracle)))
               all-oracles))
  )
)

;; Get the list of all emergency shutdowns with their details
(define-public (get-all-emergency-shutdowns)
  (let (
    (all-shutdowns (map-get-all emergency-shutdown))
  )
    (ok (map (lambda (shutdown)
                 (tuple (get id shutdown)
                        (get reason shutdown)
                        (get timestamp shutdown)))
               all-shutdowns))
  )
)

;; Get the list of all protocol parameters with their values
(define-public (get-all-protocol-parameters)
  (ok {
    protocol-fee-bp: (var-get protocol-fee-bp),
    min-deposit-amount: (var-get min-deposit-amount),
    emergency-shutdown: (var-get emergency-shutdown),
    volatility-update-frequency: (var-get volatility-update-frequency),
    max-price-impact-bp: (var-get max-price-impact-bp),
    impermanent-loss-threshold: (var-get impermanent-loss-threshold),
    impermanent-loss-coverage-bp: (var-get impermanent-loss-coverage-bp),
    dynamic-range-adjustment-factor: (var-get dynamic-range-adjustment-factor),
    price-deviation-threshold: (var-get price-deviation-threshold),
    max-dynamic-fee-increase: (var-get max-dynamic-fee-increase)
  })
)

;; Get the list of all curve types with their details
(define-public (get-all-curve-types)
  (let (
    (all-curves (map-get-all curve-types))
  )
    (ok (map (lambda (curve)
                 (tuple (get id curve)
                        (get name curve)))
               all-curves))
  )
)

;; Get the list of all pool statuses with their details
(define-public (get-all-pool-statuses)
  (let (
    (all-statuses (map-get-all pool-statuses))
  )
    (ok (map (lambda (status)
                 (tuple (get id status)
                        (get name status)))
               all-statuses))
  )
)

;; Get the list of all range statuses with their details
(define-public (get-all-range-statuses)
  (let (
    (all-ranges (map-get-all range-statuses))
  )
    (ok (map (lambda (range)
                 (tuple (get id range)
                        (get name range)))
               all-ranges))
  )
)

;; Get the list of all user positions with their details
(define-public (get-all-user-positions)
  (let (
    (all-positions (map-get-all liquidity-positions))
  )
    (ok (map (lambda (position)
                 (tuple (get position-id position)
                        (get pool-id position)
                        (get provider position)
                        (get liquidity-units position)
                        (get token-x-amount position)
                        (get token-y-amount position)
                        (get entry-price position)
                        (get entry-sqrt-price position)
                        (get entry-block position)
                        (get last-update-block position)
                        (get tick-lower position)
                        (get tick-upper position)
                        (get range-status position)
                        (get fees-earned-x position)
                        (get fees-earned-y position)
                        (get rewards-earned position)
                        (get rewards-claimed position)
                        (get il-compensation position)
                        (get is-concentrated position)))
               all-positions))
  )
)

;; Get the list of all pools with their basic details
(define-public (get-all-pools-basic)
  (let (
    (all-pools (map-get-all liquidity-pools))
  )
    (ok (map (lambda (pool)
                 (tuple (get pool-id pool)
                        (get token-x pool)
                        (get token-y pool)
                        (get reserve-x pool)
                        (get reserve-y pool)
                        (get liquidity-units pool)
                        (get curve-type pool)
                        (get base-fee-bp pool)
                        (get dynamic-fee-bp pool)
                        (get current-tick pool)
                        (get status pool)))
               all-pools))
  )
)

;; Get the list of all tokens with their basic details
(define-public (get-all-tokens-basic)
  (let (
    (all-tokens (map-get-all token-registry))
  )
    (ok (map (lambda (token)
                 (tuple (get token-id token)
                        (get name token)
                        (get token-type token)
                        (get decimals token)
                        (get is-stable token)
                        (get max-supply token)
                        (get last-price token)))
               all-tokens))
  )
)

;; Get the list of all users with their basic details
(define-public (get-all-users-basic)
  (let (
    (all-users (map-get-all user-positions))
  )
    (ok (map (lambda (user)
                 (tuple (get user user)
                        (map (lambda (position-id)
                                (unwrap! (map-get? liquidity-positions { position-id: position-id })))
                              (get position-ids user))))
               all-users))
  )
)

;; Get the list of all oracles with their basic details
(define-public (get-all-oracles-basic)
  (let (
    (all-oracles (map-get-all oracle-prices))
  )
    (ok (map (lambda (oracle)
                 (tuple (get token-id oracle)
                        (get price oracle)
                        (get twap-price oracle)
                        (get trusted oracle)
                        (get oracle-address oracle)))
               all-oracles))
  )
)

;; Get the list of all emergency shutdowns with their details
(define-public (get-all-emergency-shutdowns)
  (let (
    (all-shutdowns (map-get-all emergency-shutdown))
  )
    (ok (map (lambda (shutdown)
                 (tuple (get id shutdown)
                        (get reason shutdown)
                        (get timestamp shutdown)))
               all-shutdowns))
  )
)

;; Get the list of all protocol parameters with their values
(define-public (get-all-protocol-parameters)
  (ok {
    protocol-fee-bp: (var-get protocol-fee-bp),
    min-deposit-amount: (var-get min-deposit-amount),
    emergency-shutdown: (var-get emergency-shutdown),
    volatility-update-frequency: (var-get volatility-update-frequency),
    max-price-impact-bp: (var-get max-price-impact-bp),
    impermanent-loss-threshold: (var-get impermanent-loss-threshold),
    impermanent-loss-coverage-bp: (var-get impermanent-loss-coverage-bp),
    dynamic-range-adjustment-factor: (var-get dynamic-range-adjustment-factor),
    price-deviation-threshold: (var-get price-deviation-threshold),
    max-dynamic-fee-increase: (var-get max-dynamic-fee-increase)
  })
)

;; Get the list of all curve types with their details
(define-public (get-all-curve-types)
  (let (
    (all-curves (map-get-all curve-types))
  )
    (ok (map (lambda (curve)
                 (tuple (get id curve)
                        (get name curve)))
               all-curves))
  )
)

;; Get the list of all pool statuses with their details
(define-public (get-all-pool-statuses)
  (let (
    (all-statuses (map-get-all pool-statuses))
  )
    (ok (map (lambda (status)
                 (tuple (get id status)
                        (get name status)))
               all-statuses))
  )
)

;; Get the list of all range statuses with their details
(define-public (get-all-range-statuses)
  (let (
    (all-ranges (map-get-all range-statuses))
  )
    (ok (map (lambda (range)
                 (tuple (get id range)
                        (get name range)))
               all-ranges))
  )
)

;; Get the list of all user positions with their details
(define-public (get-all-user-positions)
  (let (
    (all-positions (map-get-all liquidity-positions))
  )
    (ok (map (lambda (position)
                 (tuple (get position-id position)
                        (get pool-id position)
                        (get provider position)
                        (get liquidity-units position)
                        (get token-x-amount position)
                        (get token-y-amount position)
                        (get entry-price position)
                        (get entry-sqrt-price position)
                        (get entry-block position)
                        (get last-update-block position)
                        (get tick-lower position)
                        (get tick-upper position)
                        (get range-status position)
                        (get fees-earned-x position)
                        (get fees-earned-y position)
                        (get rewards-earned position)
                        (get rewards-claimed position)
                        (get il-compensation position)
                        (get is-concentrated position)))
               all-positions))
  )
)

;; Get the list of all pools with their basic details
(define-public (get-all-pools-basic)
  (let (
    (all-pools (map-get-all liquidity-pools))
  )
    (ok (map (lambda (pool)
                 (tuple (get pool-id pool)
                        (get token-x pool)
                        (get token-y pool)
                        (get reserve-x pool)
                        (get reserve-y pool)
                        (get liquidity-units pool)
                        (get curve-type pool)
                        (get base-fee-bp pool)
                        (get dynamic-fee-bp pool)
                        (get current-tick pool)
                        (get status pool)))
               all-pools))
  )
)

;; Get the list of all tokens with their basic details
(define-public (get-all-tokens-basic)
  (let (
    (all-tokens (map-get-all token-registry))
  )
    (ok (map (lambda (token)
                 (tuple (get token-id token)
                        (get name token)
                        (get token-type token)
                        (get decimals token)
                        (get is-stable token)
                        (get max-supply token)
                        (get last-price token)))
               all-tokens))
  )
)

;; Get the list of all users with their basic details
(define-public (get-all-users-basic)
  (let (
    (all-users (map-get-all user-positions))
  )
    (ok (map (lambda (user)
                 (tuple (get user user)
                        (map (lambda (position-id)
                                (unwrap! (map-get? liquidity-positions { position-id: position-id })))
                              (get position-ids user))))
               all-users))
  )
)

;; Get the list of all oracles with their basic details
(define-public (get-all-oracles-basic)
  (let (
    (all-oracles (map-get-all oracle-prices))
  )
    (ok (map (lambda (oracle)
                 (tuple (get token-id oracle)
                        (get price oracle)
                        (get twap-price oracle)
                        (get trusted oracle)
                        (get oracle-address oracle)))
               all-oracles))
  )
)

;; Get the list of all emergency shutdowns with their details
(define-public (get-all-emergency-shutdowns)
  (let (
    (all-shutdowns (map-get-all emergency-shutdown))
  )
    (ok (map (lambda (shutdown)
                 (tuple (get id shutdown)
                        (get reason shutdown)
                        (get timestamp shutdown)))
               all-shutdowns))
  )
)

;; Get the list of all protocol parameters with their values
(define-public (get-all-protocol-parameters)
  (ok {
    protocol-fee-bp: (var-get protocol-fee-bp),
    min-deposit-amount: (var-get min-deposit-amount),
    emergency-shutdown: (var-get emergency-shutdown),
    volatility-update-frequency: (var-get volatility-update-frequency),
    max-price-impact-bp: (var-get max-price-impact-bp),
    impermanent-loss-threshold: (var-get impermanent-loss-threshold),
    impermanent-loss-coverage-bp: (var-get impermanent-loss-coverage-bp),
    dynamic-range-adjustment-factor: (var-get dynamic-range-adjustment-factor),
    price-deviation-threshold: (var-get price-deviation-threshold),
    max-dynamic-fee-increase: (var-get max-dynamic-fee-increase)
  })
)

;; Get the list of all curve types with their details
(define-public (get-all-curve-types)
  (let (
    (all-curves (map-get-all curve-types))
  )
    (ok (map (lambda (curve)
                 (tuple (get id curve)
                        (get name curve)))
               all-curves))
  )
)

;; Get the list of all pool statuses with their details
(define-public (get-all-pool-statuses)
  (let (
    (all-statuses (map-get-all pool-statuses))
  )
    (ok (map (lambda (status)
                 (tuple (get id status)
                        (get name status)))
               all-statuses))
  )
)

;; Get the list of all range statuses with their details
(define-public (get-all-range-statuses)
  (let (
    (all-ranges (map-get-all range-statuses))
  )
    (ok (map (lambda (range)
                 (tuple (get id range)
                        (get name range)))
               all-ranges))
  )
)

;; Get the list of all user positions with their details
(define-public (get-all-user-positions)
  (let (
    (all-positions (map-get-all liquidity-positions))
  )
    (ok (map (lambda (position)
                 (tuple (get position-id position)
                        (get pool-id position)
                        (get provider position)
                        (get liquidity-units position)
                        (get token-x-amount position)
                        (get token-y-amount position)
                        (get entry-price position)
                        (get entry-sqrt-price position)
                        (get entry-block position)
                        (get last-update-block position)
                        (get tick-lower position)
                        (get tick-upper position)
                        (get range-status position)
                        (get fees-earned-x position)
                        (get fees-earned-y position)
                        (get rewards-earned position)
                        (get rewards-claimed position)
                        (get il-compensation position)
                        (get is-concentrated position)))
               all-positions))
  )
)

;; Get the list of all pools with their basic details
(define-public (get-all-pools-basic)
  (let (
    (all-pools (map-get-all liquidity-pools))
  )
    (ok (map (lambda (pool)
                 (tuple (get pool-id pool)
                        (get token-x pool)
                        (get token-y pool)
                        (get reserve-x pool)
                        (get reserve-y pool)
                        (get liquidity-units pool)
                        (get curve-type pool)
                        (get base-fee-bp pool)
                        (get dynamic-fee-bp pool)
                        (get current-tick pool)
                        (get status pool)))
               all-pools))
  )
)

;; Get the list of all tokens with their basic details
(define-public (get-all-tokens-basic)
  (let (
    (all-tokens (map-get-all token-registry))
  )
    (ok (map (lambda (token)
                 (tuple (get token-id token)
                        (get name token)
                        (get token-type token)
                        (get decimals token)
                        (get is-stable token)
                        (get max-supply token)
                        (get last-price token)))
               all-tokens))
  )
)

;; Get the list of all users with their basic details
(define-public (get-all-users-basic)
  (let (
    (all-users (map-get-all user-positions))
  )
    (ok (map (lambda (user)
                 (tuple (get user user)
                        (map (lambda (position-id)
                                (unwrap! (map-get? liquidity-positions { position-id: position-id })))
                              (get position-ids user))))
               all-users))
  )
)

;; Get the list of all oracles with their basic details
(define-public (get-all-oracles-basic)
  (let (
    (all-oracles (map-get-all oracle-prices))
  )
    (ok (map (lambda (oracle)
                 (tuple (get token-id oracle)
                        (get price oracle)
                        (get twap-price oracle)
                        (get trusted oracle)
                        (get oracle-address oracle)))
               all-oracles))
  )
)

;; Get the list of all emergency shutdowns with their details
(define-public (get-all-emergency-shutdowns)
  (let (
    (all-shutdowns (map-get-all emergency-shutdown))
  )
    (ok (map (lambda (shutdown)
                 (tuple (get id shutdown)
                        (get reason shutdown)
                        (get timestamp shutdown)))
               all-shutdowns))
  )
)

;; Get the list of all protocol parameters with their values
(define-public (get-all-protocol-parameters)
  (ok {
    protocol-fee-bp: (var-get protocol-fee-bp),
    min-deposit-amount: (var-get min-deposit-amount),
    emergency-shutdown: (var-get emergency-shutdown),
    volatility-update-frequency: (var-get volatility-update-frequency),
    max-price-impact-bp: (var-get max-price-impact-bp),
    impermanent-loss-threshold: (var-get impermanent-loss-threshold),
    impermanent-loss-coverage-bp: (var-get impermanent-loss-coverage-bp),
    dynamic-range-adjustment-factor: (var-get dynamic-range-adjustment-factor),
    price-deviation-threshold: (var-get price-deviation-threshold),
    max-dynamic-fee-increase: (var-get max-dynamic-fee-increase)
  })
)

;; Get the list of all curve types with their details
(define-public (get-all-curve-types)
  (let (
    (all-curves (map-get-all curve-types))
  )
    (ok (map (lambda (curve)
                 (tuple (get id curve)
                        (get name curve)))
               all-curves))
  )
)

;; Get the list of all pool statuses with their details
(define-public (get-all-pool-statuses)
  (let (
    (all-statuses (map-get-all pool-statuses))
  )
    (ok (map (lambda (status)
                 (tuple (get id status)
                        (get name status)))
               all-statuses))
  )
)

;; Get the list of all range statuses with their details
(define-public (get-all-range-statuses)
  (let (
    (all-ranges (map-get-all range-statuses))
  )
    (ok (map (lambda (range)
                 (tuple (get id range)
                        (get name range)))
               all-ranges))
  )
)

;; Get the list of all user positions with their details
(define-public (get-all-user-positions)
  (let (
    (all-positions (map-get-all liquidity-positions))
  )
    (ok (map (lambda (position)
                 (tuple (get position-id position)
                        (get pool-id position)
                        (get provider position)
                        (get liquidity-units position)
                        (get token-x-amount position)
                        (get token-y-amount position)
                        (get entry-price position)
                        (get entry-sqrt-price position)
                        (get entry-block position)
                        (get last-update-block position)
                        (get tick-lower position)
                        (get tick-upper position)
                        (get range-status position)
                        (get fees-earned-x position)
                        (get fees-earned-y position)
                        (get rewards-earned position)
                        (get rewards-claimed position)
                        (get il-compensation position)
                        (get is-concentrated position)))
               all-positions))
  )
)