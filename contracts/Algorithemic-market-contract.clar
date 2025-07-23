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