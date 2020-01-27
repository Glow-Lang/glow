
Intended input for `buy_sig.glo`:

```glow
@interaction(participants=[Buyer, Seller])
function payForSignature(digest : Digest, price : Assets) {
   @Buyer deposit! price;
   @Seller @publicly const signature = sign(digest);
   withdraw! Seller <- price; }
```

Which after expansion of the `@publicly` becomes:

```glow
@interaction(participants=[Buyer, Seller])
function payForSignature(digest : Digest, price : Assets) {
   @Buyer deposit! price;
   @Seller @verifiably const signature = sign(digest);
   @Seller publish! signature;
   verify! signature;
   withdraw! Seller <- price; }
```

Intended input form (exact form to be negotiated with DrewC):

```scheme
(@at
  (interaction participants: (Buyer Seller))
  (function payForSignature ((:: digest Digest) (:: price Assets))
     (@at Buyer (deposit! price))
     (@at Seller (@at verifiably (const signature (sign digest))))
     ;; or @publicly instead of @verifiably above, and the two lines below can be omitted:
     (@at Seller (publish! signature))
     (verify! signature)
     (withdraw! Seller price)))
```

Intended output JS:

```javascript
const payForSignature_contract = ...;

async function payForSignature__Buyer (me, Seller, digest, price) {
   const interaction = await create_interaction(
     {contract: payForSignature_contract,
      parameters: {Buyer: me, Seller, digest, price},
      value: price });
   const message1 = await interaction.recv("event1");
   assert (check_signature(Seller, message1.parameters.signature, digest));
   assert (message1.value == price);
   return interaction.close(); // returns an object to query for success and out of which to notably extract the .vars.signature
}

async function payForSignature__Seller (me, Buyer, digest, price) {
   const interaction = await existing_interaction(
     {contract: payForSignature_contract,
      parameters: {Buyer, Seller: me, digest, price},
      value: price });
   // message name, parameters, value -- note that we don't need an escrow, so the value is 0 for now.
   await interaction.send("message1", {signature: make_signature(me, digest)}, 0);
   return interaction.close(); // return an object to query for success
}
```
