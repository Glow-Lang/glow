<template>
  <!-- Edit a contact and its associated identities. -->
  <q-form @submit="save"
          @reset="reset"
          class="q-pa-md">
    <q-list>
      <q-item>
        <q-input v-model="name"
                 label="Name"
                 autocorrect="off"
                 spellcheck="false" />
      </q-item>
      <q-item>
        <q-btn flat no-caps
               @click="addAddress()"
               label="Add an address"
               icon="person_add" />
      </q-item>
      <q-item>
        <q-btn flat no-caps
               v-if="cid"
               :to="{ name: 'generate-identity', params: { cid: cid, name: name } }"
               label="Generate a new identity"
               icon="contact_mail" />
      </q-item>
      <q-item v-for="(identity, index) in identities" :key="index"
              @mouseenter="toggleDeleteVisibility"
              @mouseleave="toggleDeleteVisibility">
        <q-item-section>
          <q-item-section>
            <q-input v-model="identities[index].nickname"
                     label="Nickname"
                     autocorrect="off"
                     spellcheck="false" />
          </q-item-section>
          <q-select emit-value filled map-options
                    v-model="identities[index].network"
                    :options="networks"
                    option-value="name"
                    option-label="name"
                    label="Network" />
          <q-input v-model="identities[index].address"
                   :rules="[(addr) => addr.length == 42 &&
                                      addr.substr(0, 2) == '0x' ||
                                      '0x1234ABCD... (40 hex digits)']"
                   label="Address"
                   autocorrect="off"
                   spellcheck="false"
                   style="width: 45ch" />
          <q-input v-model="identities[index].public_key"
                   label="Public Key"
                   autocorrect="off"
                   spellcheck="false"
                   style="width: 45ch" />
          <q-input readonly
                   v-model="identities[index].secret_key"
                   type="password"
                   label="Encrypted Secret Key"
                   autocorrect="off"
                   spellcheck="false"
                   style="width: 45ch" />
        </q-item-section>
        <q-item-section avatar>
          <q-btn flat round
                 @click="deleteIdentity(index)"
                 class="delete invisible"
                 color="red"
                 icon="delete" />
        </q-item-section>
      </q-item>
      <q-item>
        <q-btn type="submit"
               label="Save"
               color="primary" />
        <q-btn flat
               class="q-ml-sm"
               label="Reset"
               type="reset"
               color="primary" />
      </q-item>
    </q-list>
  </q-form>
</template>

<script>
const axios = require("axios");
export default {
    name: 'EditContact',
    props: ['cid'],
    data() {
        return {
            name: null, // model (provisional)
            identities: [], // model (provisional)
            contact: null, // from server (canonical)
            networks: [], // from server (canonical)
        }
    },
    methods: {
        addAddress() {
            this.identities.unshift({"network": "", "address": ""});
        },
        save() {
            if (this.cid) {
                axios.put("/contacts/contact/" + this.cid.toString(), {
                    name: this.name,
                    identities: this.identities,
                }).then((response) => {
                    console.log("Contact", this.cid, "updated");
                    this.$router.back();
                });
            } else {
                axios.post("/contacts/contact", {
                    name: this.name,
                    identities: this.identities,
                }).then((response) => {
                    console.log(response.data);
                    this.$router.back();
                });
            }
            false
        },
        reset() {
            this.name = this.contact.name;
            this.identities = this.contact.identities.map((x) => Object.assign({}, x));
        },
        deleteIdentity(index) {
            this.identities.splice(index, 1);
        },
        toggleDeleteVisibility(event) {
            var elts = event.target.getElementsByClassName("delete");
            for (var i = 0; i < elts.length; i++) {
                elts.item(i).classList.toggle("invisible");
            }
        },
    },
    created() {
        // Fetch contact & identities for editing.
        if (this.cid && !isNaN(parseInt(this.cid))) {
            axios.get("/contacts/contact/" + this.cid)
                 .then((response) => {
                     this.contact = response.data
                     this.reset();
                 });
        }

        // Fetch networks list.
        axios.get("/contacts/networks")
             .then((response) => {
                 this.networks = response.data;
             });
    }
}
</script>
