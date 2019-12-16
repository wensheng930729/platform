const _env = process.env.RUN_ENV || 'dev'

/*请求地址配置*/
const baseUrl = {
    // 平台前端地址
    beesrv: _env === 'dev' ? 'http://192.168.3.199:81'
        : _env === 'test' ? 'http://182.139.182.247:9162'
            : _env === 'qa' ? 'http://192.168.3.181:9162'
                // : _env === 'qa1' ? 'http://192.168.3.205:9152'
                : _env === 'qa1' ? 'http://182.139.182.247:9152'
                    : 'https://www.beesrv.com',
    //平台后端请求地址请求
    domain: _env === 'dev' ? 'http://192.168.3.199:81/bee-web'
        : _env === 'qa' ? 'http://192.168.3.181:9162/bee-web'
            : _env === 'test' ? 'http://182.139.182.247:9162/bee-web'
                // : _env === 'qa1' ? 'http://192.168.3.205:9152/bee-web'
                : _env === 'qa1' ? 'http://182.139.182.247:9152/bee-web'
                    : 'https://www.beesrv.com/bee-web',
    //中台前端地址
    console: _env === 'pro' ? 'https://console.beesrv.com'
        : _env === 'qa' ? 'http://192.168.3.181:9156'
            : _env === 'test' ? 'http://182.139.182.247:9156'
                : _env === 'qa1' ? 'http://192.168.3.205:9156'
                    : 'http://192.168.3.199:82',
    //后台前端地址
    admin: _env === 'pro' ? 'https://console.beesrv.com'
        : _env === 'qa' ? 'http://192.168.3.181:9156'
            : _env === 'test' ? 'http://182.139.182.247:9156'
                // : _env === 'qa1' ? 'http://192.168.3.205:9156'
                : _env === 'qa1' ? 'http://182.139.182.247:9156'
                    : 'http://192.168.3.199:83',
    //供应链请求地址域名
    supplychain: _env === 'dev' ? 'http://192.168.3.179'
        : _env === 'qa' ? 'http://192.168.3.181:9155'
            : _env === 'test' ? 'http://182.139.182.247:9155'
                // : _env === 'qa1' ? 'http://192.168.3.205:9155'
                : _env === 'qa1' ? 'http://182.139.182.247:9155'
                    : 'https://scf.beesrv.com',
    //OA
    oa: 'http://yyoa.scmdjt.com:6666',
    //物流
    tms: 'https://tms.beesrv.com',
    //ekp
    ekp: 'http://oa.beesrv.com:8686/autoLogin.do',
    //物联网
    ifms: _env === 'pro' ? 'https://ifms.beesrv.com'
        : _env === 'qa' ? 'http://192.168.3.181:9153'
            : 'http://182.139.182.247:9153',
    //ERP
    erp: _env === 'pro' ? 'https://erp.beesrv.com' : 'http://182.139.182.247:9804',
    //贸易
    trade: _env === 'dev' ? 'http://192.168.3.8:9151'
        : _env === 'test' ? 'http://182.139.182.247:9151'
            : _env === 'qa' ? 'http://192.168.3.181:9151'
                // : _env === 'qa1' ? 'http://192.168.3.205:9151'
                : _env === 'qa1' ? 'http://182.139.182.247:9151'
                    : 'https://go.beesrv.com',
}

export default baseUrl
