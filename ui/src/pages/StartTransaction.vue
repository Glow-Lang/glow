<template>
  <!-- Start a transaction from some source. -->
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
        <template v-if="source.nickname">
          {{ source.name }} ({{ source.nickname }})
        </template>
        <template v-else>
          {{ source.name }} ({{ source.network }}) at {{ source.address }}
        </template>
        ...
      </h3>
      <q-form>
        <q-select emit-value filled map-options
                  v-model="action"
                  :options="actions"
                  option-value="name"
                  option-label="label"
                  label="Action" />
        <q-list bordered>
          <q-item clickable
                  v-if="action == 'faucet'"
                  :to="{
                      name: 'run-transaction',
                      params: {
                          action: action,
                          source: {
                              cid: source.cid,
                              name: source.name,
                              network: source.network,
                              address: source.address
                          }
                      }
                  }">
            <q-item-label>Fund me!</q-item-label>
          </q-item>
          <q-expansion-item default-closed
                            v-else
                            v-for="contact in contacts"
                            :key="contact.cid"
                            :label="contact.name || '&lt;anonymous>'"
                            group="contacts"
                            icon="perm_identity">
            <q-list>
              <q-item v-if="contact.identities.length == 0">
                <q-item-label caption>No addresses found.</q-item-label>
              </q-item>
              <q-item clickable
                      v-else
                      v-for="(identity, index) in contact.identities"
                      :key="index"
                      :to="{
                          name: 'run-transaction',
                          params: {
                              action: action,
                              source: {
                                  cid: source.cid,
                                  name: source.name,
                                  nickname: source.nickname,
                                  network: source.network,
                                  address: source.address
                              },
                              dest: {
                                  cid: contact.cid,
                                  name: contact.name,
                                  nickname: identity.nickname,
                                  network: identity.network,
                                  address: identity.address
                              }
                          }
                      }">
                <q-item-section avatar>{{ identity.nickname }}</q-item-section>
                <q-item-section avatar><q-avatar icon="contact_mail" /></q-item-section>
                <q-item-section>{{ identity.network }}</q-item-section>
                <q-item-section>{{ identity.address }}</q-item-section>
              </q-item>
            </q-list>
          </q-expansion-item>
        </q-list>
      </q-form>
    </template>
  </q-page>
</template>

<script>
const axios = require("axios");
export default {
    props: ["source"],
    data() {
        return {
            action: "transfer-to",
            actions: [{
                name: "faucet",
                label: "Faucet..."
            }, {
                name: "transfer-to",
                label: "Transfer tokens to...",
            }, {
                name: "transfer-from",
                label: "Transfer tokens from...",
            }, {
                name: "buy-sig",
                label: "Buy signature from...",
            }, {
                name: "sell-sig",
                label: "Sell signature to...",
            }, {
                name: "rps-A",
                label: "Start a game of rock, paper, scissors with...",
            }, {
                name: "rps-B",
                label: "Join a game of rock, paper, scissors with...",
            }, {
                name: "swap-A",
                label: "Start a swap with...",
            }, {
                name: "swap-B",
                label: "Join a swap with...",
            }],
            contacts: [],
        }
    },
    created() {
        axios.get("/contacts/contacts")
             .then((response) => {
                 this.contacts = response.data;
             });
    }
}
</script>
