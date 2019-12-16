import { config } from "common"
let username =  window.g_app._store.getState().global.user.username

export default {
  //货品查询
  getSubjectMatterList: {
    api(params) {
      return `${config.supplychain}/supplychainfinance-common/subjectMatter/getSubjectMatterList`
    },
    type: "POST"
  },
  //委托采购
  postBuyApplication:{
    api:() => `${username==='12345678910'?'https://scft.beesrv.com':config.supplychain}` + '/supplychainfinance-input/entrustBuyApply/serviceApply',
    type:'POST'
  },
  //大企业委托采购
  postLargeBuyApplication:{
    api:() => `${username==='12345678910'?'https://scft.beesrv.com':config.supplychain}` + '/supplychainfinance-input/largeEntrustBuyApply/serviceApply',
    type:'POST'
  },
  //委托销售
  postSaleApplication:{
    api:() => `${username==='12345678910'?'https://scft.beesrv.com':config.supplychain}` + '/supplychainfinance-input/entrustSaleApply/serviceApply',
    type:'POST'
  },
  //金融仓储
  postStorageApplication:{
    api:() => `${username==='12345678910'?'https://scft.beesrv.com':config.supplychain}` + '/supplychainfinance-input/financeStorageApply/serviceApply',
    type:'POST'
  },
}