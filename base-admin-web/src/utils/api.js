//管理后台主要服务地址
const _env=process.env.BUILD_ENV;
// const _env = 'qa';

const domainUrl = _env === 'pro' ? "https://www.beesrv.com/bee-web/manager" :
  _env === 'qa' ? "http://192.168.3.181:9152/bee-web/manager" :
    _env === 'qa1' ? "http://192.168.3.194:8075/manager" :
      _env === 'test' ? "http://182.139.182.247:9152/bee-web/manager"
        : "http://192.168.3.179:81/bee-web/manager";

//新闻资讯模块
const newsUrl = _env === 'pro' ? "https://www.beesrv.com/bee-web/api/user" :
  _env === 'qa' ? "http://192.168.3.181:9152/bee-web/api/user" :
    _env === 'qa1' ? "http://192.168.3.205:9154/api/user" :
      _env === 'test' ? "http://182.139.182.247:9152/bee-web/api/user"
        : "http://192.168.3.179:81/bee-web/api/user";

const supply = {//供应链模块
  domain: _env === 'pro' ? "https://scf.beesrv.com" :
    _env === 'qa' ? "http://192.168.3.181:9155" :
      _env === 'qa1' ? "http://192.168.3.205:9155" :
        _env === 'test' ? "http://182.139.182.247:9155"
          : "http://192.168.3.179"
}

const path = {
  login: '/login', //登录
  uploadFile: 'https://www.beesrv.com/bee-web/api/files/upload', //上传图片正式地址
  // uploadFile: 'http://192.168.3.179:81/bee-web/api/files/upload', //上传图片测试地址
  // uploadFile: 'http://192.168.3.99:8075/api/files/upload', //上传图片
  enterpriseCount: '/counts', //企业条数
  enterpriseUnhandleData: '/unhandled_checks', //未审核
  enterprisePassedData: '/passed_checks', //已通过
  enterpriseRefusedData: '/refused_checks', //未通过
  enterpriseDetails: '/getInfo', //企业详情
  enterpriseAction: '/review', //审核企业操作  ??
  enterpriseSave: '/saveInfo', //保存企业修改  11
  news: '/news/query', //条件查询资讯列表
  newsCount: '/news/count', //资讯总数
  newsDetails: '/news/inner', //资讯详情
  newsAdd: '/news/publish', //新增资讯
  newsEdit: '/news/update', //更新资讯
  newsDelete: '/news/delete', //批量删除资讯  11
}

export { domainUrl, newsUrl, path, supply };