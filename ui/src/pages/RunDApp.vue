<template>
  <!-- Run a transaction. -->
  <q-page class="flex flex-center">
    <div class="q-pa-md q-gutter-sm" style="width:100%">
      <q-breadcrumbs>
        <q-breadcrumbs-el icon="home" to="/" />
        <q-breadcrumbs-el icon="apps" to="/dapps" />
        <q-breadcrumbs-el icon="play_arrow" label="Run DApp" />
      </q-breadcrumbs>
    </div>
    <template v-if="dapp && my_role">
      <h3 style="display: inline-flex; justify-content: center; flex-basis: 100%">
        <template v-if="dapp">{{ dapp }}</template>
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
    </template>
  </q-page>
</template>

<script>
const axios = require("axios");
export default {
    props: ["dapp", "assets", "inputs", "my_role", "participants", "params"],
    data() {
        return {
            txid: null,
            output: null,
            status: null,
        }
    },
    created() {
        if (this.dapp) {
            this.startTransaction();
        }
    },
    methods: {
        startTransaction() {
            console.warn("Posting transaction");
            axios.post("/contacts/transaction", {
                action: "run-dapp",
                args: {
                    dapp: this.dapp,
                    assets: this.assets,
                    my_role: this.my_role,
                    participants: this.participants,
                    params: this.params,
                    input: Object.values(this.inputs).join(''), // FIXME
                }
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
