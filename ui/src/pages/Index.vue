<template>
<q-page class="flex flex-center">
  <div class="text-h3" style="width: 100%; text-align: center;">
    Welcome to Glow!
  </div>
  <q-list padding class="text-h4">
    <q-item>
      <q-icon name="people_alt" />&ensp;
      <router-link to="/contacts">Contacts</router-link>
    </q-item>
    <q-item>
      <q-icon name="apps" />&ensp;
      <router-link to="/dapps">DApps</router-link>
    </q-item>
    <q-item>
      <q-icon name="link" />&ensp;
      <router-link to="/networks">Networks</router-link>
    </q-item>
  </q-list>
</q-page>
</template>
