#前端代码规范
###①　组件的文件命名与组件名一致。
###②　同一模块内组件用index.js导出。
###③　组件代码内export default组件统一命名为组件名。
###④　注释(api.js、model文件夹下、)
###⑤　变量命名语义化
###⑤　函数、判断语句、循环语句前空一行
###⑤　文件夹命名使用 大驼峰、
###⑤　公用组件 注释需写明接收参数，抛出方法所带数据标明。
###⑤　不能有大量冗余。


#环境配置
###①　npm install -g umi(如未安装umi依赖)
###②　npm install
###③　启动本地:umi dev (默认监听8000端口) 
###③　启动本地:port=8888 umi dev (监听8888端口启动) 
###④　打包(dist文件夹) :
####                        npm run build (开发环境打包，默认打包)
####                        npm run build:pro (正式环境打包)
####                        npm run build:qa (测试qa打包)
####                        npm run build:qa1 (测试qa1打包)
                     

#地址修改
###①　后台访问域名修改路径:/src/utils/api.js  (  baseUrl为bussiness模块,userUrl为user模块,)