-> = λa:*.λb:*.∀x:a.b;
idT = λa:*.-> a a;
id = λa:*.λx:a.x;
id : ∀a:*.idT a;
nat = ∀a:*.idT (idT a);

comp = λa:*.λb:*.λc:*. λf:(-> a b).λg:(-> b c). λx:a.g (f x);
comp : ∀a:*.∀b:*.∀c:*. -> (-> a b) (-> (-> b c) (-> a c));

const = λa:*.λb:*.λx:a.λy:b.x;
const : ∀a:*.∀b:*.(-> a (-> b a));
ignore = λa:*.λb:*.λx:a.λy:b.y;

zero = λa:*.const (idT a) (idT a) (id a);
zero : nat;

succ = λn:nat.λa:*.λf:(idT a).λx:a.n a f (f x);
succ : idT nat;

add = λn:nat.λm:nat.n nat succ m;
add : -> nat (idT nat);

eq = λa:*. λb:a.λc:a. ∀f:(∀x:a.*). -> (f b) (f c);
eq : ∀a:*. ∀b:a.∀c:a. *;

eqRefl = λa:*. λb:a. λf:(∀x:a.*). λfb:(f b). fb;
eqRefl : ∀a:*. ∀b:a. eq a b b;

eqTrans = λa:*. λb:a.λc:a.λd:a. λebc:(eq a b c).λecd:(eq a c d). 
          λf:(∀x:a.*). comp (f b) (f c) (f d) (ebc f) (ecd f);
eqTrans : ∀a:*. ∀b:a.∀c:a.∀d:a. -> (eq a b c) (-> (eq a c d) (eq a b d));

eqSymm = λa:*. λb:a.λc:a. λebc:(eq a b c).
         ebc (λx:a. eq a x b) (eqRefl a b);
eqSymm : ∀a:*. ∀b:a.∀c:a. -> (eq a b c) (eq a c b);