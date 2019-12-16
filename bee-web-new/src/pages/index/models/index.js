import { getSellsInfo, getBuysInfo } from '../services/index'

export default {
  namespace: 'index',
  state: {
    sells: [],
    buys: [],
    number: {}
  },
  effects: {
    //获取现货信息
    *getSellsInfo({ payload, callback }, { call, put }) {
      const response = yield call(getSellsInfo, payload)
      if (response.code === 1) {
        yield put({
          type: 'setSellsInfo',
          payload: response.data
        })
        callback && callback(response)
      }
    },

    //获取询价信息
    *getBuysInfo({ payload, callback }, { call, put }) {
      const response = yield call(getBuysInfo, payload)
      if (response.code === 1) {
        yield put({
          type: 'setBuysInfo',
          payload: response.data
        })
        callback && callback(response)
      }
    },

    *getDetails({ payload, error }, { call, put }) {
      const res = yield call(getDetails, payload)
      if (res.code === 0) {
        yield put({
          type: 'setDetails',
          payload: res.object
        })
      } else {
        error && error(res.message)
      }
    }
  },
  reducers: {
    setSellsInfo(state, action) {
      //获取现货信息
      return {
        ...state,
        sells: action.payload
      }
    },

    setBuysInfo(state, action) {
      //获取询价信息
      return {
        ...state,
        buys: action.payload
      }
    },

    setDetails(state, action) {
      //设置新闻详情
      return {
        ...state,
        details: action.payload
      }
    },

    setNumber(state, action) {
      return {
        ...state,
        number: action.payload
      }
    }
  }
}
