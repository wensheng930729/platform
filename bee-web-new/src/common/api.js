export default {
  //登录相关接口
  loginCommon:{
    //账号密码登录
    login:{
      api:(params) =>`/authPlatformUser/login`,
      type: "POST"
    },
    //验证码登录
    loginCode:{
      api:(params) =>`/authPlatformUser/verification/code/login`,
      type: "POST"
    },
    //注销登录
    loginOut:{
      api:()=>`/authPlatformUser/logout`,
      type: "POST"
    }
  },
  //忘记密码相关接口
  reset:{
    //账号密码登录
    resetPassword:{
      api:(params) =>`/authPlatformUser/resetPassword`,
      type: "POST"
    },
    //获取修改密码验证码
    getValidateCode: {
      api: (params) => `/authPlatformUser/getValidateCodeWithHasAccount`,
      type: "POST"
    }
  },
  // 验证码相关接口
  getCode:{
    //用于存在的用户请求验证码
    getValidateCodeWithHasAccount:{
      api:(params) =>`/authPlatformUser/getValidateCodeWithHasAccount`,
      type: "POST"
    },
    //校验验证码
    validate:{
      api:(params) =>`/authPlatformUser/validate`,
      type: "POST"
    }
  },
  //注册相关接口
  register: {
    //用户注册接口
    regist: {
      api: (params) => `/authPlatformUser/register`,
      type: "POST"
    },
    //注册验证码判定
    registCode: {
      api: (params) => `/authPlatformUser/validate`,
      type: "POST"
    },
    //获取注册验证码
    getRegistCode: {
      api: (params) => `/authPlatformUser/getReisterCode`,
      type: "POST"
    },
  },
  //首页相关接口
  home: {
    //获取首页数量
    getAllNumbers: {
      api: (params) => `/number/getAllNumbers`,
      type: "GET"
    },
    //获取现货大厅数据
    getAllSaleByType: {
      api: (params) => `/trading/product/getAllSaleByType?size=${params}`,
      type: "GET"
    },
    //获取询价大厅数据
    getAllPurchaseByType: {
      api: (params) => `/trading/product/getAllPurchaseByType?size=${params}`,
      type: "GET"
    },
  },
  //用户信息相关接口
  user: {
    //获取个人信息
    getSelfInfo: {
      api: () => `/authPlatformUser/getSelfInfo`,
      type: "GET"
    },
    //获取内部系统用户信息
    getUserInfo: {
      api: () => `/authPlatformUser/userInfo`,
      type: "GET"
    },
    //修改个人信息
    modifySelfInfo: {
      api: () => `/authPlatformUser/modifySelfInfo`,
      type: "POST"
    },
    //获取用户的企业列表
    enterprises: {
      api: () => `/authPlatformUserEnterprise/loginUser/enterprises`,
      type: "GET"
    },
    //查询用户申请认证还未通过的企业信息
    authenticatedList: {
      api: (phone) => `/enterprisesCheck/authenticatedList?phone=${phone}`,
      type: "GET"
    },
    //切换企业
    changeCompany: {
      api: (params) => `/authPlatformUser/enterprise/${params}`,
      type: "POST"
    }
  },
  //公用接口整理
  common: {
    //获取地址选择信息
    findRegionByParentId: {
      api: (params) => `/api/user/region/findRegionByParentId/${params}`,
      type: "GET"
    },
    //上传文件接口
    upload: {
      api: () => `https://www.beesrv.com/bee-web/api/files/uploadFile`,
      type: "POST"
    }
  },
  //企业相关接口
  business: {
    //获取行业信息字典
    getIndustry: {
      api: (params) => `/api/dict/list?type=${params}`,
      type: "GET"
    },
    //重新认证企业信息查询接口
    enterpriseCheckInfo: {
      api: (params) => `/enterprisesCheck/enterpriseCheckInfo?checkId=${params}`,
      type: "GET"
    },
    //认证企业接口
    enterpriseRegister: {
      api: () =>`/authEnterprise/register`,
      type: "POST"
    },
    //获取企业认证未通过的信息
    getAuthenticatedList: {
      api: (params) => `/enterprisesCheck/authenticatedList?phone=${params}`,
      type: "GET"
    },
    //获取所有已认证的企业
    getEnterprise: {
      api: () => `/authEnterprise`,
      type: "GET"
    },
    //再次认证企业接口
    reAuthentication: {
      api: () => `/enterprisesCheck/reAuthentication`,
      type: "POST"
    },
    //撤回企业认证
    revoke: {
      api: () => `/enterprisesCheck/revoke`,
      type: "POST"
    }
  },
  //联系方式填写
  addUser: {
    addUserMsg: {
      api: "/bee-ifms/zConsult/add",
      type: "post"
    }
  },
}