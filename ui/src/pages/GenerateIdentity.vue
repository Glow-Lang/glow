<template>
  <!-- Ask glow to generate a fresh identity using a transaction. -->
  <q-page class="flex flex-center">
    <div class="q-pa-md q-gutter-sm" style="width:100%">
      <q-breadcrumbs>
        <q-breadcrumbs-el icon="home" to="/" />
        <q-breadcrumbs-el icon="people_alt" to="/contacts" />
        <q-breadcrumbs-el v-if="cid && name"
                          icon="perm_identity"
                          :label="name"
                          :to="{name: 'contact', params: {cid: cid}}" />
        <q-breadcrumbs-el icon="vpn_key" label="Create new identity" />
      </q-breadcrumbs>
    </div>
    <template v-if="cid && network && nickname && generate">
      <h3 style="display: inline-flex; justify-content: center; flex-basis: 100%">
        Generate new identity for {{ nickname }} on {{ network }}
      </h3>
      <div v-if="txid && output">
        <div style="font-size: small; overflow: scroll; max-height: 300px; width: 550px;">
          <pre>{{output}}</pre>
        </div>
        <div v-if="status == -1" style="color: red">Generation timed out.</div>
        <div v-if="status == 0" style="color: green">Generation completed successfully.</div>
        <div v-else-if="status" style="color: orangered">Exited with status {{ status }}.</div>
        <div v-else><q-linear-progress indeterminate color="warning" class="q-mt-md" /></div>
      </div>
    </template>
    <q-form v-else>
      <q-list>
        <q-item>
          <q-input v-model="nickname"
                   label="Nickname"
                   :rules="[(nick) => nick && nick.length > 0 || 'Nickname must not be empty']"
                   autocorrect="off"
                   spellcheck="false" />
          <q-select emit-value filled map-options
                    v-model="network"
                    :options="networks"
                    option-value="name"
                    option-label="name"
                    label="Network" />
        </q-item>
        <q-item>
          <q-btn @click="generateIdentity()"
                   label="Generate"
                   type="submit"
                   color="primary" />
        </q-item>
      </q-list>
    </q-form>
  </q-page>
</template>

<script>
const axios = require("axios");
export default {
    props: ["cid", "name"],
    data() {
        return {
            action: "generate-identity",
            generate: false,
            network: null,
            networks: [],
            nickname: null,
            txid: null,
            output: null,
            status: null,
        }
    },
    created() {
        axios.get("/contacts/networks")
             .then((response) => {
                 this.networks = response.data;
             });
    },
    methods: {
        generateIdentity() {
            this.generate = true;
            axios.post("/contacts/transaction", {
                action: this.action,
                args: {
                    nickname: this.nickname,
                }
            }).then((response) => {
                const txn = response.data;
                this.txid = txn.txid;
                console.log("Transaction", this.txid, "started");
                this.pollOutput();
            });
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
                         if (this.status == 0) {
                             const identity = JSON.parse(this.output);
                             if (identity) {
                                 if (identity.nickname == this.nickname &&
                                     identity.address &&
                                     identity.public_key) {
                                     console.log("Got new identity", identity);
                                     this.saveIdentity(identity);
                                 } else {
                                     console.error("Bad identity", identity);
                                 }
                             }
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
        },
        saveIdentity(identity) {
            axios.post("/contacts/contact/" + this.cid.toString(), {
                network: this.network,
                address: identity.address,
                nickname: identity.nickname,
                public_key: identity.public_key,
                secret_key_path: identity.secret_key_path,
            }).then((response) => {
                console.log("Identity added");
                this.$router.back();
            });
        }
    }
}
</script>
