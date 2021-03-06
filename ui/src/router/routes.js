const routes = [
    {
        path: '/',
        component: () => import('layouts/MainLayout.vue'),
        children: [
            { path: '', component: () => import('pages/Index.vue') },
            { path: 'contact/:cid', name: 'contact', props: true, component: () => import('pages/Contact.vue') },
            { path: 'contacts', name: 'contacts', component: () => import('pages/Contacts.vue') },
            { path: 'create-contact', name: 'create-contact', component: () => import('pages/CreateContact.vue') },
            { path: 'dapps', name: 'dapps', props: true, component: () => import('pages/DApps.vue') },
            { path: 'edit-contact/:cid', name: 'edit-contact', props: true, component: () => import('pages/EditContact.vue') },
            { path: 'generate-identity', name: 'generate-identity', props: true, component: () => import('pages/GenerateIdentity.vue') },
            { path: 'networks', component: () => import('pages/Networks.vue') },
            { path: 'run-dapp', name: 'run-dapp', props: true, component: () => import('pages/RunDApp.vue') },
        ]
    }
]

export default routes
