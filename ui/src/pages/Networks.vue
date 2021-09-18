<template>
  <!-- List known networks. -->
  <q-page class="flex flex-center">
    <div class="q-pa-md q-gutter-sm" style="width:100%">
      <q-breadcrumbs>
        <q-breadcrumbs-el icon="home" to="/" />
        <q-breadcrumbs-el icon="link" label="Networks" />
      </q-breadcrumbs>
    </div>
    <div class="q-pa-md">
      <q-list bordered>
        <q-btn no-caps unelevated
               icon="add_link"
               label="Add new network"
               :to="'/add-network'" />
        <q-expansion-item default-closed
            v-for="network in networks"
            :key="network.name"
            :label="network.name"
            group="networks"
            icon="link"
            header-class="text-primary">
          <q-card>
            <q-card-section>{{ network.description }}</q-card-section>
            <q-card-section>{{ network.uri }}</q-card-section>
          </q-card>
        </q-expansion-item>
      </q-list>
    </div>
  </q-page>
</template>

<script>
const axios = require("axios");
export default {
    data() {
        return {
            networks: [],
        }
    },
    created() {
        axios.get("/contacts/networks")
             .then((response) => {
                 this.networks = response.data;
             });
    }
}
</script>
