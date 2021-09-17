<template>
  <!-- Run a transaction. -->
  <q-page class="flex flex-center">
    <div class="q-pa-md q-gutter-sm" style="width:100%">
      <q-breadcrumbs>
        <q-breadcrumbs-el icon="home" to="/" />
        <q-breadcrumbs-el icon="people_alt" to="/contacts" />
        <q-breadcrumbs-el v-if="source"
                          icon="perm_identity"
                          :label="source.name || source.nickname || '&lt;anonymous>'"
                          :to="{name: 'contact', params: {cid: source.cid}}" />
        <q-breadcrumbs-el v-if="dest"
                          icon="perm_identity"
                          :label="dest.name || dest.nickname || '&lt;anonymous>'"
                          :to="{name: 'contact', params: {cid: dest.cid}}" />
        <q-breadcrumbs-el icon="play_arrow" label="Run transaction" />
      </q-breadcrumbs>
    </div>
    <template v-if="source && action">
      <h3 style="display: inline-flex; justify-content: center; flex-basis: 100%">
        <template v-if="source && dest && tokens">
          {{ source.name || source.nickname || "&lt;anonymous>" }} ({{ source.network }})
          <template v-if="action == 'transfer-to'">&rarr;</template>
          <template v-else-if="action == 'transfer-from'">&larr;</template>
          <template v-else>{{ action }}</template>
          {{ dest.name || dest.nickname || '&lt;anonymous>' }} ({{ dest.network }})
        </template>
        <template v-if="action == 'faucet'">
          Fund {{ source.name || source.nickname || '&lt;anonymous>' }} ({{ source.network }})
        </template>
      </h3>
      <div v-if="txid && output">
        <div style="font-size: small; overflow: scroll; max-height: 300px; width: 550px;">
          <pre>{{output}}</pre>
        </div>
        <div v-if="status == -1" style="color: red">Transaction timed out.</div>
        <div v-else-if="status == 0" style="color: green">Transaction completed successfully.</div>
        <div v-else-if="status" style="color: orangered">Exited with status {{ status }}.</div>
        <div v-else><q-linear-progress indeterminate color="warning" class="q-mt-md" /></div>
      </div>
      <div v-else-if="action == 'faucet'">
        Turning on the faucet...
      </div>
      <div v-else-if="action == 'transfer-to' || action == 'transfer-from'">
        <q-form>
          <q-list>
            <q-item>
              <q-input v-model="amount"
                       label="Amount"
                       autocorrect="off"
                       spellcheck="false" />
              <q-select v-model="token"
                        :options="tokens"
                        label="Token" />
            </q-item>
            <q-item>
              <q-btn @click="transfer()"
                   label="Transfer"
                   type="submit"
                   color="primary" />
            </q-item>
          </q-list>
        </q-form>
      </div>
      <div v-else-if="action == 'buy-sig' || action == 'sell-sig'">
        <q-form @submit="buySig">
          <q-list>
            <q-item>
              <q-input v-model="amount"
                       label="Amount"
                       autocorrect="off"
                       spellcheck="false" />
              <q-select v-model="token"
                        :options="tokens"
                        label="Token" />
            </q-item>
            <q-item>
              <q-input v-model="digest"
                       :rules="[(digest) => digest.length >= 40 &&
                                            digest.substr(0, 2) == '0x' ||
                                            '0x1234ABCD...']"
                       label="Signature digest"
                       autocorrect="off"
                       spellcheck="false"
                       style="width: 45ch" />
            </q-item>
            <q-item>
              <q-btn type="submit"
                     :label="action == 'buy-sig' ? 'Buy signature' : 'Sell signature'"
                     color="primary" />
            </q-item>
          </q-list>
        </q-form>
      </div>
      <div v-else-if="action == 'rps-A' || action == 'rps-B'">
        <q-form @submit="rps">
          <q-list>
            <q-item>
              <q-input v-model="amount"
                       label="Wager amount"
                       autocorrect="off"
                       spellcheck="false" />
              <q-select v-model="token"
                        :options="tokens"
                        label="Token" />
            </q-item>
            <q-item>
              <q-select emit-value filled map-options
                        v-model="hand"
                        :options="[{label: 'Rock', value: '0'},
                                   {label: 'Paper', value: '1'},
                                   {label: 'Scissors', value: '2'}]"
                        label="Hand">
                <template v-slot:prepend>
                  <q-icon v-if="hand === '0'" name="fas fa-hand-rock" />
                  <q-icon v-else-if="hand === '1'" name="fas fa-hand-paper" />
                  <q-icon v-else-if="hand === '2'" name="fas fa-hand-scissors" />
                </template>
              </q-select>
            </q-item>
            <q-item>
              <q-btn type="submit" label="Play!" color="primary" />
            </q-item>
          </q-list>
        </q-form>
      </div>
      <div v-else-if="action == 'swap-A' || action == 'swap-B'">
        <q-form @submit="swap">
          <q-list>
            <q-item>
              <q-input v-model="t_amount"
                       label="I'll swap..."
                       autocorrect="off"
                       spellcheck="false" />
              <q-select v-model="t_token"
                        :options="tokens"
                        label="Token" />
            </q-item>
            <q-item>
              <q-input v-model="u_amount"
                       label="For..."
                       autocorrect="off"
                       spellcheck="false" />
              <q-select v-model="u_token"
                        :options="tokens"
                        label="Token" />
            </q-item>
            <q-item>
              <q-btn type="submit" label="Swap!" color="primary" />
            </q-item>
          </q-list>
        </q-form>
      </div>
      <div v-else>Unsupported action: {{ action }}</div>
    </template>
  </q-page>
</template>

<script>
const axios = require("axios");
export default {
    props: ["source", "action", "dest"],
    data() {
        return {
            amount: null,
            token: null,
            tokens: [],
            networks: [],
            txid: null,
            output: null,
            status: null,
            digest: null, // buy/sell sig
            hand: "0", // rps: rock
            t_amount: null, // swap
            t_token: null, // swap
            u_amount: null, // swap
            u_token: null, // swap
        }
    },
    created() {
        axios.get("/contacts/networks")
             .then((response) => {
                 this.networks = response.data;
                 if (this.source) {
                     this.token = this.networks
                                      .find((network) => network.name == this.source.network)
                                      .native_token;
                 }
             });
        axios.get("/contacts/assets")
             .then((response) => {
                 this.tokens = response.data;
                 this.tokens.sort();
             });

        // Turn on faucets right away, no need to wait for args.
        if (this.action == "faucet") {
            this.faucet();
        }
    },
    methods: {
        faucet() {
            this.startTransaction({
                source: this.source,
            })
        },
        transfer() {
            this.startTransaction({
                source: this.source,
                dest: this.dest,
                amount: this.amount,
                token: this.token,
            })
        },
        buySig() {
            this.startTransaction({
                source: this.source,
                dest: this.dest,
                amount: this.amount,
                token: this.token,
                digest: this.digest,
            })
        },
        rps() {
            if (this.action == "rps-A") {
                this.startTransaction({
                    a: this.source,
                    b: this.dest,
                    amount: this.amount,
                    token: this.token,
                    hand: this.hand,
                })
            } else if (this.action == "rps-B") {
                this.startTransaction({
                    a: this.dest,
                    b: this.source,
                    amount: this.amount,
                    token: this.token,
                    hand: this.hand,
                })
            }
        },
        swap() {
            if (this.action == "swap-A") {
                this.startTransaction({
                    a: this.source,
                    b: this.dest,
                    t_amount: this.t_amount,
                    t_token: this.t_token,
                    u_amount: this.u_amount,
                    u_token: this.u_token,
                })
            } else if (this.action == "swap-B") {
                this.startTransaction({
                    a: this.dest,
                    b: this.source,
                    t_amount: this.t_amount,
                    t_token: this.t_token,
                    u_amount: this.u_amount,
                    u_token: this.u_token,
                })
            }
        },
        startTransaction(args) {
            axios.post("/contacts/transaction", {
                action: this.action,
                args: args
            }).then(this.transactionStarted);
            false
        },
        transactionStarted(response) {
            const txn = response.data;
            this.txid = txn.txid;
            console.log("Transaction", this.txid, "started");
            this.pollOutput();
        },
        pollOutput() {
            const interval = setInterval(() => {
                axios.get("/contacts/transaction/" + this.txid.toString(), {})
                     .then((response) => {
                         const txn = response.data;
                         if (this.txid != txn.txid) {
                             throw("Transaction ID mismatch");
                         }
                         this.output = txn.output;
                         this.status = txn.status;
                         if (this.status) {
                             clearInterval(interval);
                         }
                     })
                     .catch((error) => {
                         if (error.response) {
                             console.log(error.response)
                         } else if (error.request) {
                             console.log(error.request);
                         } else {
                             console.log("Error:", error);
                         }
                         clearInterval(interval);
                     });
            }, 100);
        }
    }
}
</script>
