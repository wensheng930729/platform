import router from "umi/router";
import {
  loginPass,
  loginCode,
  getPersonInfo,
  getPersonEnterprises,
  logout,
  getAuthenticatedList
} from "../services/index";
import { config } from "@/common";
import { message } from "antd";
import { utils} from "common";

const {clearUserInfo} = utils
const { baseUrl } = config;

export default {
  namespace: "global",
  state: {
    login: false,
    user: {
      address: "",
      department: null,
      email: "",
      fixtel: "",
      head: "",
      id: null,
      is_active: null,
      is_invite: null,
      nickname: "",
      orgId: null,
      org_name: "",
      org_nickname: "",
      phone: "",
      post: "",
      qq: "",
      region: {
        city: {},
        county: {},
        province: {}
      },
      roleId: null,
      roleName: "",
      username: ""
    },
    company: [],
    authenticatedList:[],
    visible:false
  },
  effects: {
    *login({ payload, callback }, { call, put }) {
      const response = yield call(loginPass, payload);
      if (response.code === 0 && response.data.sysToken) {
        yield put({
          type: "setLoginInfo"
        });
        if(response.data.org_name){
          localStorage.setItem("companyName", response.data.org_name);
        }
        localStorage.setItem("username", response.data.username);
        localStorage.setItem("sysToken", response.data.sysToken);
        callback && callback(response.code, response.message,response.data);
      } else {
        clearUserInfo()
        callback && callback(response.code, response.message);
        return;
      }
    },
    *loginCode({ payload, callback }, { call, put }) {
      const response = yield call(loginCode, payload);
      if (response.code === 0 && response.data.sysToken) {
        localStorage.setItem("sysToken", response.data.sysToken);
        yield put({
          type: "setLoginInfo"
        });
        callback && callback(response.code, response.message,response.data);
      } else {
        clearUserInfo()
        callback && callback(response.code, response.message);
        return;
      }
    },
    *getSelfInfo({ payload, callback }, { call, put }) {
      const response = yield call(getPersonInfo, payload);
      //code1
      if (response.code === 0 && response.data) {
        if(response.data.org_name){
          localStorage.setItem("companyName", response.data.org_name);
        }
        localStorage.setItem("username", response.data.username);
        yield put({
          type: "setUserInfo",
          payload: response.data
        });
        callback && callback(response.code, response.message, response.data);
      } else {
        clearUserInfo()
        callback && callback(response.code, response.message);
      }
    },
    *getEnterprises({ payload, callback }, { call, put }) {
      const response = yield call(getPersonEnterprises, payload);
      //code1
      if (response.code === 0 && response.data) {
        yield put({
          type: "setUserCompanyInfo",
          payload: response.data
        });
        callback && callback(response.code, response.message, response.data);
      } else {
        clearUserInfo()
        callback && callback(response.code, response.message);
      }
    },
    *getPersonAuthenticatedList({ payload, callback }, { call, put }) {
      const response = yield call(getAuthenticatedList, payload);
      //code1
      if (response.code === 0 && response.data) {
        yield put({
          type: "setUsertAuthenticatedListInfo",
          payload: response.data
        });
        callback && callback(response.code, response.message, response.data);
      } else {
        clearUserInfo()
        callback && callback(response.code, response.message);
      }
    },
    *userOut({ payload, callback }, { call, put }) {
      const response = yield call(logout, payload);
      if (response.code === 0) {
        clearUserInfo()
        sessionStorage.removeItem('loginSuccessNextPage')
        localStorage.removeItem('tradeType')
        sessionStorage.clear()
        yield put({
          type: "resetInfo"
        });
        callback && callback(response.code, response.message, response.data);
      } else {
        callback && callback(response.code, response.message);
      }
    },
    *reSetUser({ payload, callback }, { call, put }) {
      yield put({
        type: "resetInfo"
      })
    },
    *reSetVisible({ payload, callback }, { call, put }) {
      yield put({
        type: "setVisible"
      })
    },
  },
  reducers: {
    setLoginInfo(state, action) {
      return {
        ...state,
        login: true
      };
    },
    setUserInfo(state, action) {
      return {
        ...state,
        login: true,
        user: {...state.user, ...action.payload}
      };
    },
    setUserCompanyInfo(state, action) {
      return {
        ...state,
        login: true,
        company: action.payload
      };
    },
    setUsertAuthenticatedListInfo(state, action) {
      return {
        ...state,
        login: true,
        authenticatedList: action.payload
      };
    },
    resetInfo(state, action) {
      return {
        login: false,
        user: {
          address: "",
          department: null,
          email: "",
          fixtel: "",
          head: "",
          id: null,
          is_active: null,
          is_invite: null,
          nickname: "",
          orgId: null,
          org_name: "",
          org_nickname: "",
          phone: "",
          post: "",
          qq: "",
          region: {
            city: {},
            county: {},
            province: {}
          },
          roleId: null,
          roleName: "",
          username: ""
        },
        company: [],
        authenticatedList:[]
      };
    },
    setVisible(state, action) {
      if(state.visible === true){
        return {
          ...state,
          visible:false
        };
      }
      return {
        ...state,
        visible:true
      };
    }
  }
};
