<template>
  <q-layout view="lHh Lpr lFf">
    <q-header elevated>
      <q-toolbar>
        <q-btn
          flat
          dense
          round
          icon="menu"
          aria-label="Menu"
          @click="leftDrawerOpen = !leftDrawerOpen"
        />

        <q-toolbar-title>
          Glow: Decentralized Applications across Blockchains
        </q-toolbar-title>

        <div>GloUI: 0.5.0 </div>
      </q-toolbar>
    </q-header>

    <q-drawer
      v-model="leftDrawerOpen"
      show-if-above
      bordered
      content-class="bg-grey-1"
    >
      <q-list>
        <q-item-label
          header
          class="text-grey-8"
        >
          Essential Links
        </q-item-label>
        <EssentialLink
          v-for="link in essentialLinks"
          :key="link.title"
          v-bind="link"
        />
      </q-list>
    </q-drawer>

    <q-page-container>
      <router-view />
    </q-page-container>
  </q-layout>
</template>

<script>
import EssentialLink from 'components/EssentialLink.vue'

const linksData = [
  {
    title: 'Glow Homepage',
    caption: 'glow-lang.org',
    icon: 'school',
    link: 'https://glow-lang.org/'
  },
  {
    title: 'Glow Documentation',
    caption: 'gitlab code',
    icon: 'code',
    link: 'https://glow-lang.org/#Takemetothecode'
  },
  {
    title: 'Discord Chat Channel',
    caption: 'invitation',
    icon: 'chat',
    link: 'https://discord.gg/pPMcqD6hWz'
  }
]

export default {
  name: 'MainLayout',
  components: { EssentialLink },
  created: function () {},
  data () {
    return {
      leftDrawerOpen: false,
      essentialLinks: linksData,
    }
  }
}
</script>
