module.exports = {
  packagerConfig: {
    name: 'stats4ROI',
    appBundleId: 'com.roially.stats4roi',
    appCategoryType: 'public.app-category.education',
    osxSign: false,
    osxNotarize: false
  },
  makers: [
    {
      name: '@electron-forge/maker-zip',
      platforms: ['darwin']
    },
    {
      name: '@electron-forge/maker-dmg',
      config: {
        name: 'stats4ROI'
      }
    }
  ]
};
