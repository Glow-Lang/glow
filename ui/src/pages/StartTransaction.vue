<template>
  <!-- Start a transaction from some source address.
    -- Choose the DApp, assign roles & parameters, and run it. -->
  <q-page class="flex flex-center">
    <div class="q-pa-md q-gutter-sm" style="width:100%">
      <q-breadcrumbs>
        <q-breadcrumbs-el icon="home" to="/" />
        <q-breadcrumbs-el icon="people_alt" to="/contacts" />
        <q-breadcrumbs-el v-if="source"
                          icon="perm_identity"
                          :label="source.name"
                          :to="{name: 'contact', params: {cid: source.cid}}" />
        <q-breadcrumbs-el icon="not_started" label="Start transaction" />
      </q-breadcrumbs>
    </div>
    <template v-if="source">
      <h3 style="display: inline-flex; justify-content: center; flex-basis: 100%">
        <template v-if="source.name && source.nickname">
          {{ source.name }} ({{ source.nickname }})
        </template>
        <template v-else-if="source.nickname">
          {{ source.nickname }}
        </template>
        <template v-else>
          {{ source.name }} ({{ source.network }}) at {{ source.address }}
        </template>
        ...
      </h3>
    </template>
  </q-page>
</template>

<script>
const axios = require("axios");
export default {
    props: ["source"],
    data() {
        return {
            action: null,
            actions: [],
            assets: {}, // name → token
            contacts: [],
            dapps: {}, // name → path
            params: {}, // param → value
            participants: {}, // role → participant
            schemas: {}, // name → schema
            schema: null, // current param & input schema
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
                     this.dapps[dapp["name"]] = dapp["path"];
                 });
                 this.actions = Object.keys(this.dapps);
             });
    },
    methods: {
        getSchema() {
            if (this.action in this.schemas) {
                this.schema = this.schemas[this.action];
            } else {
                console.log("Fetching schema for", this.action);
                axios.get("/contacts/schema/" +
                          encodeURIComponent(this.dapps[this.action] || this.action))
                     .then((response) => {
                         this.schema = this.schemas[this.action] = response.data;
                     });
            }
            return this.schema;
        },
        nonTrivialAssets(entry) {
            return ('assets' in entry &&
                    entry['assets'].length > 0 &&
                    !(entry['assets'].length === 1 &&
                      entry['assets'][0] === 'DefaultToken'))
        }
    }
}
</script>
