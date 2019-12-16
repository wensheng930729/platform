const path = require('path')
const _env = process.env.BUILD_ENV || 'dev'
const _version = +new Date()
console.log('当前打包环境：' + _env)
console.log('当前时间戳版本号：' + _version)

export default {
  plugins: [
    [
      'umi-plugin-react',
      {
        dva: {
          immer: true
        },
        antd: true,
        library: 'react',
        dynamicImport: true,
        fastClick: true,
        routes: {
          exclude: [/components/, /apis/, /services/, /models/]
        }
      }
    ]
  ],
  targets: {
    ie: 7,
    chrome: 49,
    firefox: 45,
    safari: 10,
    edge: 13,
    ios: 10
  },
  hash: true,
  extraBabelPlugins: [
    ['import', { libraryName: 'antd', libraryDirectory: 'es', style: true }]
  ],
  alias: {
    components: path.resolve(__dirname, '../src/components/'),
    layouts: path.resolve(__dirname, '../src/layouts/'),
    common: path.resolve(__dirname, '../src/common/'),
    assets: path.resolve(__dirname, '../src/assets/'),
    services: path.resolve(__dirname, '../src/services/')
  },
  // routes: pageRoutes
  // ,
  proxy: {
    '/api': {
      target: 'http://jsonplaceholder.typicode.com/',
      changeOrigin: true,
      pathRewrite: { '^/api': '' }
    }
  },
  disableCSSModules: true,
  define: {
    'process.env.RUN_ENV': _env,
    'process.env.HASH_VERSION': _version
  }
}
