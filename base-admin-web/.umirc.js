const path = require('path');
const _env= process.env.BUILD_ENV||'dev';
console.log('当前打包环境：'+_env)
export default {
  history: 'browser',
  plugins: [
    ['umi-plugin-react', {
      antd: true,
      routes: {
        exclude: [
          /model\.(j|t)sx?$/,
          /service\.(j|t)sx?$/,
          /models\//,
          /components\//,
          /services\//,
        ],
      },
    }],
  ],
  alias: {
    components: path.resolve(__dirname, '../src/components/'),
    // layouts: path.resolve(__dirname, 'src/layouts'),
    utils: path.resolve(__dirname, '../src/utils/'),
    assets: path.resolve(__dirname, '../src/assets/'),
  }, define: {
    "process.env.BUILD_ENV":_env,
  }
};
