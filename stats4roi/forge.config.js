const { version } = require('./package.json');
const { signApp, findApps } = require('./scripts/adhoc-sign');

module.exports = {
  packagerConfig: {
    name: 'stats4ROI',
    appBundleId: 'com.roially.stats4roi',
    appCategoryType: 'public.app-category.education',
    // No Developer ID: the postPackage hook below applies a valid ad-hoc seal
    // instead, so downloads are not reported as "damaged". See scripts/adhoc-sign.js.
    osxSign: false,
    osxNotarize: false
  },
  hooks: {
    // Runs after packaging and before the ZIP/DMG makers, so both ship the sealed app.
    postPackage: async (_forgeConfig, { platform, outputPaths }) => {
      if (platform !== 'darwin') return;
      for (const appPath of findApps(outputPaths)) signApp(appPath);
    }
  },
  makers: [
    {
      name: '@electron-forge/maker-zip',
      platforms: ['darwin']
    },
    {
      name: '@electron-forge/maker-dmg',
      config: {
        name: `stats4ROI-${version}`
      }
    }
  ]
};
