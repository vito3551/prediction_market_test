type bid_map = (nat, nat) map
type auction_map = (string, bid_map) map
let fee : nat = 1n
 

let empty_qps_map : (nat, nat) map = Map.empty
let empty_auction_map : (address, bid_map) map = Map.empty
let empty_Q_map : (address, nat) map = Map.empty

type auction = {
    q_total : nat; (* total quantity bought by everyone *)
    min_qps_total : nat; (* maybe change the name to sum_of_min_qps_val *) (* This is the sum of each users min val of (quantity times probability) *)
    qps_total : bid_map; (* map with all quantity * probability *)
}

type active_market = {
    clearing_prices : bid_map; (* the price of each outcome after the clearing stage *)
}

type market_state =
    | Auction of auction
    | Trading of active_market

type market = {
    num_outcomes : nat;
    outcome_token_ids : (nat, nat) map; (* ids of each outcome token *)
    lqt_token_id : nat; (* Liquidity token *)
    state : market_state;
}

type market_map = ( nat, market ) big_map

type bet = {
    quantity : nat;
    probabilities : bid_map;
}

type bet_id = {
    market_id : nat;
    owner : address;
}

type bet_map = ( bet_id, bet ) big_map

type market_storage = {
    markets : market_map;
    bets : bet_map;
}

type return = (operation list) * market_storage

let empty_market_map : ( nat, market ) big_map = Big_map.empty
let empty_bet_map : ( bet_id, bet ) big_map = Big_map.empty

(* function that returns the market_record *)
let get_market ( market_id, market_map : nat * market_map ) : market =
    match ( Big_map.find_opt market_id market_map ) with
    | Some m -> m
    | None -> ( failwith "error_NO_SUCH_MARKET" : market )

let get_auction_data (market : market) : auction =
    match market.state with 
    | Auction a -> a
    | Trading _ -> (failwith "error_AUCTION_CLOSED" : auction)

(* returns a map that contains a specific user's quantity * probability *)
let calculate_Qp (usr_Q, usr_probs : nat * bid_map) : bid_map =
    let multiqp : (nat * nat) -> nat = fun (_, proba : nat * nat) -> 
        (Bitwise.shift_left (proba * usr_Q) 48n) in
    (Map.map multiqp usr_probs : bid_map)

(* adds each user's quantity * probability for each outcome to the market total *)
let calculate_Qp_total (usr_qp , old_qp_total : bid_map * bid_map) : bid_map =
    let new_qp_tot : (nat * nat) -> nat = fun (i, qp_val : nat * nat) ->
        let prev_qps_tot  = match Map.find_opt i old_qp_total with
        | Some x -> x
        | None -> 0n in
        prev_qps_tot + qp_val in
    (Map.map new_qp_tot usr_qp : bid_map)

(* calculate the price of each outcome after the auction stage *)
let calculate_clearing_price (q_total, qps_total : nat * bid_map) : bid_map =
    let new_clear_price : (nat * nat) -> nat = fun (_, qp_val_tot : nat * nat) ->
        (qp_val_tot / q_total) in 
    (Map.map new_clear_price qps_total : (nat, nat) map)

(* returns the smallest value from a map *)
let get_min_val (m : (nat, nat) map) : nat = 
    let min (x, y : nat * nat) = if x < y then x else y in
    let folded = fun (i, j : nat * (nat * nat)) ->
        let mv = min (i, j.1) in
        (i * 0n) + mv in
    (Map.fold folded m (Bitwise.shift_left 10000000n 48n))  

let new_bet ( bet_id, bet, market_storage : bet_id * bet * market_storage ) : return =
    let market_id = bet_id.market_id in
    // get the market_record
    let market_record = get_market (market_id, market_storage.markets) in //Big_map.find_opt market_id market_storage.markets in
    // get auction_record_data
    let auction = get_auction_data market_record in
    // check if user has already made a bet
    if Big_map.mem bet_id market_storage.bets then
        (failwith "error_USER_HAS_ALREADY_MADE_BET" : return)
    else 
    let new_quantity = auction.q_total + bet.quantity in
    // calculate quantity times probability for a user
    let usr_qp = calculate_Qp (bet.quantity, bet.probabilities) in
    // calculate new total value of all users (quantity times probability)
    let new_qp_total = calculate_Qp_total (usr_qp, auction.qps_total) in
    // get the minimum value of a users (quantity times probability)
    let usr_min_qp_val = get_min_val usr_qp in
    let new_min_qps_total = auction.min_qps_total + usr_min_qp_val in
    // update auction running totals with bet
    let auction = { auction with 
        q_total = new_quantity;
        qps_total = new_qp_total;
        min_qps_total = new_min_qps_total;
        } in

    // add new auction_record to market_record
    let market_record = {market_record with state = Auction(auction)} in
    // add new market_record to market_map
    let market_map = Big_map.update market_id (Some market_record) market_storage.markets in
    // Add bet to bet_map
    let bet_map = Big_map.update bet_id (Some bet) market_storage.bets in
    // Update market_storage
    let market_storage = { market_storage with 
        bets = bet_map;
        markets = market_map;
        } in
    
    (([] : operation list), market_storage)

let end_auction (market_id, market_storage : nat * market_storage) : return =
    // get the market_record
    let market_record = get_market (market_id, market_storage.markets) in
    // get auction_record_data
    let auction = get_auction_data market_record in
    // calculate clearing prices
    let clearing_prices = calculate_clearing_price (auction.q_total, auction.qps_total : nat * bid_map) in

    (([] : operation list), market_storage)
    


(* get the map that contains a specific user's probabilities 
let get_probabilities (addr : address) (ms : market_storage) : bid_map = 
    match Map.find_opt addr ms.auction_bids with
    | None -> (failwith "error_PROBS_DOESNT_EXIST" : bid_map) 
    | Some p -> p *)

(* check that a user's probabilities belongs to [0,1] *)
let check_p (prob : (nat, nat) map) : unit = 
    let predicate = fun (_, j : nat * nat) -> assert (j >= 0n && j <= 100n)
    in Map.iter predicate prob

(* sum all of the values in a map *)
let sum_prob (prob : (nat, nat) map) : nat = 
    let folded = fun (i, proba : nat * (nat * nat)) -> (i + proba.1)in 
    (Map.fold folded prob 0n)

(* check that a user's probabilities sum to one *)
let check_prob_sum_to_one (prob : (nat, nat) map) : bool =
    let sum_probs : nat = sum_prob prob in
    (sum_probs = 100n)  

(* get the map that contains a specific user's quantity * probability *)
let get_Qps (addr, qpmap : address * (address, bid_map) map) : bid_map = 
    match Map.find_opt addr qpmap with
    | None -> (failwith "error_QPS_DOESNT_EXIST" : bid_map) (* empty_qps_map *)
    | Some qp_map -> qp_map

(* get a specific user's purchased quantity *)
let get_Q (addr, auctionQs : address * (address, nat) map) : nat = 
    match Map.find_opt addr auctionQs with
    | None -> (failwith "error_Q_DOESNT_EXIST" : nat)
    | Some q -> q

(* adds a specific user's purchased quantity to the total quantity 
let update_q_total (addr : address) (ms : market_storage) : market_storage =
    let user_Q = get_Q addr ms in
    {ms with q_total = ms.q_total + user_Q} *)

(* adds each user's quantity * probability for each outcome to the market total 
let update_qps_total (addr : string) (ms : market_storage) : bid_map = 
    let qps_tot = ms.qps_total in
    let qps_user = get_Qps addr ms in
    let new_qp_tot : (nat * nat) -> nat = fun (i, qp_val : nat * nat) ->
        let prev_qps_tot  = match Map.find_opt i qps_tot with
        | Some x -> x
        | None -> 0n in
        prev_qps_tot + qp_val in
    (Map.map new_qp_tot qps_user : bid_map) *)

//let new_bet ( bet, market_storage : bet * market_storage ) : operation list * market_storage =
    // check bet validity
    // generate FA12 payment operation
    // check if user has already made a bet
    // merge bets if yes
    // update auction running totals with bet
    // save bet
    // pack things up and return


(* ALLOCATIONS after the auction stage *)

(* get the map that contains a specific user's allocation for each outcome *)
let get_allocations (addr, market_allocations : address * (address, bid_map) map) : bid_map =
    match Map.find_opt addr market_allocations with
    | None -> empty_qps_map (* (failwith "error_USR_ALLOCATION_DOESNT_EXIST" : bid_map) *)
    | Some a -> a

(* calculate the allocation for each outcome that a user ends up with *)
let calculate_allocation (qps_usr, clr_prices : bid_map * bid_map) : (nat, nat) map =
    let new_alloc : (nat * nat) -> nat = fun (i, qp_val : nat * nat) -> (
        let spec_price = match (Map.find_opt i clr_prices) with
        | Some p -> p
        | None -> 0n in
        ((Bitwise.shift_left qp_val 48n) / spec_price) ) in (* To use int-math just do: ((Bitwise.shift_left qp_val 48n) / spec_price) *) (* Without int-math do: ((qp_val ) / spec_price) *)
    (Map.map new_alloc qps_usr : bid_map)

(* sum of allocation * clearing price for a user *)
let sum_alloc_times_price (alloc_m, clr_prices : bid_map * bid_map) : nat =
    let agg = fun (i, q_map : nat * (nat * nat)) ->
        let spec_price = match (Map.find_opt q_map.0 clr_prices) with
        | Some p -> p
        | None -> (failwith "error_PRICE_DOESNT_EXIST_IN_CleraingPriceMap" : nat) in
        i + (q_map.1 * spec_price) in
    (Map.fold agg alloc_m 0n) 




(* for a specific user get the min value of quantity * probability and add it together with the other users min value 
let total_min_qps (addr : string) (ms : market_storage) : market_storage = 
    let usr_Qp = get_Qps addr ms in
    let usr_min_Qp = get_min_val usr_Qp in
    {ms with min_qps_total = ms.min_qps_total + usr_min_Qp} *)

(* SWAP *)

(* calculate the avilable quantity of each outcome that will be in the uniswap pool *)
let calculate_uniswap_market (sum_of_min_qps_val, clearing_prices : nat * bid_map) : bid_map =
    let uniswap_market = fun (_, price : nat * nat) ->
        ((Bitwise.shift_left sum_of_min_qps_val 48n) / price) in //(Bitwise.shift_left (min_Qp_tot / price) 48n)
    (Map.map uniswap_market clearing_prices)


let pb0 : bid_map = Map.literal [(0n,10n); (1n,75n); (2n, 15n)]
let pb1 : bid_map = Map.literal [(0n, 15n); (1n, 70n); (2n, 15n)]
let pb2 : bid_map = Map.literal [(0n, 5n); (1n, 65n); (2n, 30n)]
let me : string = "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"
let orka : bid_map = Map.remove 0n pb2

type people = (string * string * string)
let users : people = ("u1", "u2", "u3")
let u1 : string = "u1"

let my_ms : market_storage = {
    markets = empty_market_map;
    bets = empty_bet_map;
} 

type parameter =
      Increment of int
    | Decrement of int
    | Reset

let main (action, ms : parameter * market_storage) : return =
    let _ = action in
    (([] : operation list), ms)
    



    
    