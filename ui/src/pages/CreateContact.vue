<template>
  <!-- Create a new contact. -->
  <q-page class="flex flex-center">
    <div class="q-pa-md q-gutter-sm" style="width:100%">
      <q-breadcrumbs>
        <q-breadcrumbs-el icon="home" to="/" />
        <q-breadcrumbs-el icon="people_alt" to="/contacts" />
        <q-breadcrumbs-el icon="person_add" label="Create contact" />
      </q-breadcrumbs>
    </div>
    <EditContact />
  </q-page>
</template>

<script>
import EditContact from "components/EditContact.vue"
export default {
    name: 'CreateContact',
    components: { EditContact }
}
</script>
