<template>
  <!-- Choose a DApp, assign roles, parameters, and inputs from the schema. -->
  <q-page class="flex flex-center">
    <div class="q-pa-md q-gutter-sm" style="width: 100%">
      <q-breadcrumbs>
        <q-breadcrumbs-el icon="home" to="/" />
        <q-breadcrumbs-el icon="apps" to="/dapps" />
      </q-breadcrumbs>
    </div>
    <q-form class="justify-center" style="width: 66%">
      <q-card>
        <q-select filled
                  label="DApp"
                  v-model="dapp"
                  :options="dapps"
                  @input="getSchema">
          <template v-slot:no-option>
            <q-item>
              <q-item-section class="text-grey">No DApps found</q-item-section>
            </q-item>
          </template>
          <template v-slot:selected>
            <q-item>
              <template v-if="dapp">
                <q-item-section>{{ dapp }}</q-item-section>
              </template>
              <template v-else>
                <q-item-section class="text-blue">Select a DApp...</q-item-section>
              </template>
            </q-item>
          </template>
        </q-select>
        <q-card-section v-for="(entry, index) in schema" :key="index">
          <q-card-section v-if="'assets' in entry"> <!-- v-if="nonTrivialAssets(entry)" -->
            <div class="text-h6">Assets</div>
            <q-select v-for="(asset, index) in entry['assets']"
                      v-model="assets[asset]"
                      :label="asset"
                      :key="index"
                      :options="tokens" />
          </q-card-section>
          <q-card-section v-if="'params' in entry">
            <div class="text-h6">Parameters</div>
            <q-input v-for="(param, index) in entry['params']"
                     v-model="params[param]"
                     :label="param"
                     :key="index" />
          </q-card-section>
          <q-card-section v-if="'participants' in entry">
            <div class="text-h6">Participants</div>
            <div v-for="(role, index) in entry['participants']" :key="index">
              <q-select v-model="participants[role]"
                        :label="role"
                        :options="contacts.flatMap(contact => contact.identities)"
                        option-label="nickname"
                        option-value="address" />
              <q-radio v-model="my_role"
                       :disabled="!participants[role] || !participants[role].secret_key"
                       :val="role"
                       label="My Identity" />
            </div>
          </q-card-section>
          <q-card-section v-if="'var' in entry && 'participant' in entry && entry['participant'] == my_role">
            <div class="text-h6">{{ entry['var'] }}: {{ entry['type'] }}</div>
            <q-input v-model="inputs[entry['var']]"
                     :label="entry['tag']" />
                <!-- :rules="switch (entry['type']) ..." -->
          </q-card-section>
        </q-card-section>
        <q-card-actions v-if="dapp">
          <q-card-section>
            <q-btn :to="{
                       name: 'run-dapp',
                       params: {
                           dapp: dapp,
                           assets: assets,
                           my_role: my_role,
                           participants: participants,
                           params: params,
                       }
                   }">Run {{ dapp }}</q-btn>
          </q-card-section>
        </q-card-actions>
      </q-card>
    </q-form>
  </q-page>
</template>

<script>
const axios = require("axios");
export default {
    props: ["source"],
    data() {
        return {
            assets: {}, // name → token
            contacts: [],
            dapp: null,
            dapps: [],
            dapp_paths: {}, // name → path
            inputs: {}, // var → value
            my_role: null,
            params: {}, // param → value
            participants: {}, // role → participant
            schemas: {}, // name → schema
            schema: null,
            tokens: [],
        }
    },
    created() {
        axios.get("/contacts/assets")
             .then((response) => {
                 this.tokens = response.data;
                 this.tokens.sort();
             });
        axios.get("/contacts/contacts")
             .then((response) => {
                 this.contacts = response.data;
             });
        axios.get("/contacts/applications")
             .then((response) => {
                 response.data.forEach((dapp) => {
                     this.dapp_paths[dapp["name"]] = dapp["path"];
                     this.dapps = Object.keys(this.dapp_paths);
                 });
             });
    },
    methods: {
        // Fetch the schema for the current dapp.
        getSchema() {
            if (!this.dapp) {
                // No dapp, no schema.
                this.schema = null;
            } else if (this.dapp in this.schemas) {
                // Cache hit.
                this.schema = this.schemas[this.dapp];
                this.reset();
            } else {
                // Cache miss: fetch & cache schema.
                console.log("Fetching schema for", this.dapp);
                axios.get("/contacts/schema/" +
                          encodeURIComponent(this.dapp_paths[this.dapp] || this.dapp))
                     .then((response) => {
                         this.schema = this.schemas[this.dapp] = response.data;
                         this.reset();
                     });
            }
        },

        // Reset schema-dependent data.
        reset() {
            this.assets = {}
            this.params = {}
            this.participants = {}
        },

        // Predicate: does the schema entry have a non-trivial asset list?
        nonTrivialAssets(entry) {
            return ("assets" in entry &&
                    entry["assets"].length > 0 &&
                    !(entry["assets"].length === 1 &&
                      entry["assets"][0] === "DefaultToken"))
        }
    }
}
</script>
