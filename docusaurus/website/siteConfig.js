const parseYaml = require("js-yaml").safeLoad;
const path = require("path");
const fs = require("fs");

function findMarkDownSync(startPath) {
  const result = [];
  const files = fs.readdirSync(path.join(__dirname, startPath));
  files.forEach(val => {
    const fPath = path.join(startPath, val);
    const stats = fs.statSync(fPath);
    if (stats.isDirectory()) {
      result.push({
        title: val,
        path: fPath,
      });
    }
  });
  return result;
}

function loadYaml(fsPath) {
  return parseYaml(fs.readFileSync(path.join(__dirname, fsPath), "utf8"));
}

function loadMD(fsPath) {
  return fs.readFileSync(path.join(__dirname, fsPath), "utf8");
}

const tools = loadYaml("./tools.yml");

const toolsMD = findMarkDownSync("../docs/tools/");
toolsMD.forEach(tool => {
  tool.install = loadMD(`${tool.path}/install.md`);
  tool.usage = loadMD(`${tool.path}/usage.md`);
});

// List of projects/orgs using your project for the users page.
const users = [
  {
    caption: 'User1',
    // You will need to prepend the image path with your baseUrl
    // if it is not '/', like: '/test-site/img/docusaurus.svg'.
    image: '/img/docusaurus.svg',
    infoLink: 'https://www.facebook.com',
    pinned: true,
  },
];

const repoUrl = 'https://github.com/scalacenter/bloop';
const siteConfig = {
  title: 'Bloop', // Title for your website.
  tagline: 'Compile, test and run Scala code fast',
  url: 'https://your-docusaurus-test-site.com', // Your website URL
  repoUrl,
  baseUrl: '/', // Base URL for your project */
  // For github.io type URLs, you would set the url and baseUrl like:
  //   url: 'https://facebook.github.io',
  //   baseUrl: '/test-site/',

  // Used for publishing and more
  projectName: 'bloop',
  organizationName: 'ch.epfl.scala',
  // For top-level user or org sites, the organization is still the same.
  // e.g., for the https://JoelMarcey.github.io site, it would be set like...
  //   organizationName: 'JoelMarcey'

  // For no header links in the top nav bar -> headerLinks: [],
  headerLinks: [
    {page: 'setup', label: 'Setup'},
    {doc: 'doc1', label: 'Docs'},
    {page: 'help', label: 'Community'},
    {blog: true, label: 'Blog'},
    {search: true},
    {href: repoUrl, label: 'GitHub'},
  ],

  tools,
  toolsMD,

  // If you have users set above, you add it here:
  users,

  /* path to images for header/footer */
  headerIcon: 'img/orca-whale-white.svg',
  footerIcon: 'img/docusaurus.svg',
  favicon: 'img/favicon.png',

  /* Colors for website */
  colors: {
    // primaryColor: 'rgb(39, 55, 71)',
    // primaryColor: 'rgb(27, 45, 63)',
    primaryColor: 'rgb(17, 42, 68)',

    // secondaryColor: '#05A5D1',
    secondaryColor: '#1EC2EF',

    // heroBackgroundColor: 'rgb(67, 93, 119)',
    // heroBackgroundColor: 'rgb(73, 91, 110)',
    // heroBackgroundColor: 'rgb(39, 59, 80)',
    heroBackgroundColor: 'rgba(14, 40, 68, 0.94)',

    tintColor: '#005068',
    backgroundColor: '#f5fcff',
  },

  /* Custom fonts for website */
  /*
  fonts: {
    myFont: [
      "Times New Roman",
      "Serif"
    ],
    myOtherFont: [
      "-apple-system",
      "system-ui"
    ]
  },
  */

  // This copyright info is used in /core/Footer.js and blog RSS/Atom feeds.
  copyright: `Copyright © ${new Date().getFullYear()} Your Name or Your Company Name`,

  highlight: {
    // Highlight.js theme to use for syntax highlighting in code blocks.
    theme: 'default',
  },

  // Add custom scripts here that would be placed in <script> tags.
  scripts: ['https://buttons.github.io/buttons.js'],

  // On page navigation for the current documentation page.
  onPageNav: 'separate',
  // No .html extensions for paths.
  cleanUrl: true,

  // Open Graph and Twitter card images.
  ogImage: 'img/docusaurus.png',
  twitterImage: 'img/docusaurus.png',

  // Show documentation's last contributor's name.
  // enableUpdateBy: true,

  // Show documentation's last update time.
  // enableUpdateTime: true,

  // You may provide arbitrary config keys to be used as needed by your
  // template. For example, if you need your repo's URL...
  //   repoUrl: 'https://github.com/facebook/test-site',

  algolia: {
    apiKey: 'cf5bcb37b134346182da2be3f5e0a76b',
    indexName: 'bloop_scala',
    algoliaOptions: {
      facetFilters: [ "version:VERSION" ] 
    }
  },
};

module.exports = siteConfig;
