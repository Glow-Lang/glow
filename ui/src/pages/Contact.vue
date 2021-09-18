<template>
  <!-- Display one contact and its identities. -->
  <q-page class="flex flex-center">
    <div class="q-pa-md q-gutter-sm" style="width:100%">
      <q-breadcrumbs>
        <q-breadcrumbs-el icon="home" to="/" />
        <q-breadcrumbs-el icon="people_alt" to="/contacts" />
        <q-breadcrumbs-el v-if="contact" icon="perm_identity" :label="contact.name" />
      </q-breadcrumbs>
    </div>
    <template v-if="contact">
      <div class="q-pa-md">
        <h3 @mouseenter="toggleDeleteVisibility" @mouseleave="toggleDeleteVisibility">
          {{ contact.name || "&lt;anonymous>" }}
          <q-btn flat round class="delete invisible" color="red" icon="delete" @click="confirmDeleteContact" />
        </h3>
        <q-dialog v-model="confirm">
          <q-card>
            <q-card-section>
              <div class="text-h6">Really delete contact {{ contact.name }}?</div>
            </q-card-section>
            <q-card-section v-if="contact.identities.length == 1">
              This will also delete one associated identity.
            </q-card-section>
            <q-card-section v-else-if="contact.identities.length > 1">
              This will also delete {{ contact.identities.length }} associated identities.
            </q-card-section>
            <q-card-actions align="right">
              <q-btn flat label="Cancel" color="primary" v-close-popup />
              <q-btn flat label="Delete" color="primary" v-close-popup @click="deleteContact" />
            </q-card-actions>
          </q-card>
        </q-dialog>

        <q-separator />
        <q-list>
          <q-item v-if="contact.identities.length == 0">
            <q-item-label caption>No addresses found.</q-item-label>
          </q-item>
          <q-item clickable
                  :to="{
                      name: 'start-transaction',
                      params: {
                          source: {
                              cid: contact.cid,
                              name: contact.name,
                              network: identity.network,
                              address: identity.address,
                              nickname: identity.nickname
                          }
                      }
                  }"
                  v-for="(identity, index) in contact.identities" :key="index">
            <q-item-section avatar>{{ identity.nickname }}</q-item-section>
            <q-item-section avatar><q-avatar icon="contact_mail" /></q-item-section>
            <q-item-section avatar>{{ identity.network }}</q-item-section>
            <q-item-section>{{ identity.address }}</q-item-section>
          </q-item>
          <q-item-label class="text-right">
            <q-separator /><br />
            <q-btn no-caps rounded unelevated
                   color="primary"
                   icon="edit"
                   label="Edit contact"
                   :to="{name: 'edit-contact', params: {cid: contact.cid}}" />
          </q-item-label>
        </q-list>
      </div>
    </template>
  </q-page>
</template>

<script>
const axios = require("axios");
export default {
    props: ['cid'],
    data() {
        return {
            contact: null,
            confirm: false,
        }
    },
    methods: {
        confirmDeleteContact() {
            this.confirm = true;
        },
        deleteContact() {
            if (this.confirm) {
                console.log("Delete contact", this.cid);
                axios.delete("/contacts/contact/" + this.cid)
                     .then((response) => {
                         console.log("Contact", this.cid, "deleted");
                         this.$router.back();
                     });
            }
        },
        toggleDeleteVisibility(event) {
            var elts = event.target.getElementsByClassName("delete");
            for (var i = 0; i < elts.length; i++) {
                elts.item(i).classList.toggle("invisible");
            }
        },
    },
    created() {
        if (this.cid && !isNaN(parseInt(this.cid))) {
            console.log("Fetching contact", this.cid);
            axios.get("/contacts/contact/" + this.cid)
                 .then((response) => {
                     this.contact = response.data;
                 });
        }
    }
}
</script>
