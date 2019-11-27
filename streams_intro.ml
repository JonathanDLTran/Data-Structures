let rec zeros  = 0 :: zeros
let rec ones = 1 :: ones

let rec a = 1 :: b 
and b = 2 :: a

type 'a stream = 
  | Cons of 'a * (unit -> 'a stream)

let rec from n = 
  Cons (n, (fun () -> from (n + 1)))

let nats = from 0

let hd (Cons (n, _)) = n
let tl (Cons (_, t)) = t ()

let rec nth n stream = 
  if n = 0 then hd stream
  else nth (n - 1) (tl stream)

let rec take n stream = 
  if n = 0 then []
  else (hd stream) :: take (n - 1) (tl stream)

let rec drop n stream = 
  if n = 0 then stream
  else drop (n - 1) (tl stream)

let rec square stream = 
  Cons ((hd stream) * (hd stream), fun () -> stream |> tl |> square)

let rec add stream1 stream2 = 
  Cons ((hd stream1) + (hd stream2), fun () -> add (tl stream1) (tl stream2))

let rec apply_op op stream1 stream2 = 
  Cons (op (hd stream1) (hd stream2), fun () -> apply_op op (tl stream1) (tl stream2))

let rec map f stream = 
  Cons (f (hd stream), fun () -> map f (tl stream))

let rec map2 f stream1 stream2 = 
  Cons (f (hd stream1) (hd stream2), fun () -> map2 f (tl stream1) (tl stream2))

let init f = map f nats

let rec filter f stream = 
  let head = hd stream in 
  if f head then Cons (head, fun () -> filter f (tl stream))
  else filter f (tl stream)

let rec fold_finite f init stream n = 
  if n = 0 then init
  else fold_finite f (f init (hd stream)) (tl stream) (n - 1)

let rec infinite_fold f init stream = 
  infinite_fold f (f init (hd stream)) (tl stream)

let rec thunk_fold f init stream = 
  let init' = f init (hd stream) in 
  Cons (init', fun () -> thunk_fold f init' (tl stream))

let thunk_for_all p = thunk_fold (fun init n -> init && (p n)) true 

let thunk_exists p = thunk_fold (fun init n -> init || (p n)) false

let rec mem x stream = 
  if hd stream = x then true
  else mem x (tl stream)

let rec find p stream = 
  let head = hd stream in 
  if p head then head
  else find p (tl stream)

let rec split stream = 
  let (a, b) = hd stream in 
  (Cons (a, fun () -> stream |> tl |> split |> fst), 
   Cons (b, fun () -> stream |> tl |> split |> snd))

let combine = 
  map2 (fun a b -> (a, b))

let square' = map (fun n -> n * n)
let sum' = map2 (+)

let rec nats' = Cons (0, fun () -> map (fun n -> n + 1) nats')
let rec nats'' = 
  Cons (0, fun () ->
      Cons (1, fun () ->
          map (fun n -> n + 2) nats''))

let rec nacci = 
  Cons (1, fun () ->
      Cons (1, fun () ->
          add nacci (tl nacci)))

let rec lucas = 
  Cons (2, fun () ->
      Cons (1, fun () ->
          add lucas (tl lucas)))

let sieve stream = 
  let head = hd stream in 
  filter (fun n -> n = head || n mod head <> 0) stream

let rec sieve_of_eratosthenes stream = 
  let sieved = sieve stream in 
  Cons ((hd sieved), fun () -> sieve_of_eratosthenes (tl sieved))

let primes = 
  nats |> tl |> tl |> sieve_of_eratosthenes

let nth_prime n  = 
  nth n primes