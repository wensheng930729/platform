import { getSubjectMatterList, postBuyApplication, postSaleApplication, postStorageApplication, postLargeBuyApplication } from "./services"


export default {
  namespace: 'supplyChain',
  state: {
    user: {}
  },
  effects: {
    * getSubjectMatterList({ payload, callback }, { call, put }) {
      const res = yield call(getSubjectMatterList, payload);
      callback && callback(res)
    },
    * postBuyApplication({ payload, callback }, { call, put }) {
      const res = yield call(postBuyApplication, payload);
      callback && callback(res)
    },
    * postLargeBuyApplication({ payload, callback }, { call, put }) {
      const res = yield call(postLargeBuyApplication, payload);
      callback && callback(res)
    },
    * postSaleApplication({ payload, callback }, { call, put }) {
      const res = yield call(postSaleApplication, payload);
      callback && callback(res)
    },
    * postStorageApplication({ payload, callback }, { call, put }) {
      const res = yield call(postStorageApplication, payload);
      callback && callback(res)
    },
  },
  reducers: {

  }
}