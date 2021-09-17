<template>
  <!-- Edit one contact and its identities. -->
  <q-page class="flex flex-center">
    <div class="q-pa-md q-gutter-sm" style="width:100%">
      <q-breadcrumbs>
        <q-breadcrumbs-el icon="home" to="/" />
        <q-breadcrumbs-el icon="people_alt" to="/contacts" />
        <q-breadcrumbs-el icon="edit" label="Edit contact" />
      </q-breadcrumbs>
    </div>
    <EditContact :cid="cid" />
  </q-page>
</template>

<script>
import EditContact from "components/EditContact.vue"
export default {
    name: 'EditContactPage',
    components: { EditContact },
    props: ['cid'],
}
</script>
