import {
  getNewsList,
  getDetails
} from '../services/index';

export default {
  namespace: 'news',
  state: {
    newsList: [],
    details: {}
  },
  effects: {
    *getNewsList({ payload, callback }, { call, put }) {
      const response = yield call(getNewsList, payload);
      if (response.code === 0) {
        yield put({
          type: 'setNewsList',
          payload: response.data.content
        });
        callback && callback(response)
      }
    },
    * getDetails({ payload, success, error }, { call, put }) {
      const res = yield call(getDetails, payload);
      if (res.code === 0) {
        yield put({
          type: 'setDetails',
          payload: res.data,
        });
        success && success();
      } else {
        error && error(res.message)
      }
    },
  },
  reducers: {
    setNewsList(state, action) {//设置新闻列表
      return {
        ...state,
        newsList: action.payload
      };
    },
    setDetails(state, action) {//设置新闻详情
      return {
        ...state,
        details: action.payload
      };
    },
  },
}
