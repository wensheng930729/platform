(window["webpackJsonp"]=window["webpackJsonp"]||[]).push([[23],{"7c/T":function(e,t,a){"use strict";var l=a("7rOj"),r=a("SyOF");Object.defineProperty(t,"__esModule",{value:!0}),t["default"]=void 0,a("+L6B");var n=l(a("2/Rp"));a("sRBo");var d=l(a("kaz8"));a("14J3");var s=l(a("BMrR"));a("jCWc");var o=l(a("kPKH"));a("/zsF");var u=l(a("PArb"));a("5NDa");var i=l(a("5rEg"));a("Pwec");var c=l(a("CtXQ"));a("miYZ");var f=l(a("tsqr")),m=l(a("M6Zy")),p=l(a("iyqV")),g=l(a("utfd")),h=l(a("o7c0")),y=l(a("ZnYF")),E=l(a("sMup"));a("y8nQ");var v,C,w,b=l(a("Vl3Y")),k=r(a("COVA")),N=(l(a("bIAp")),l(a("xIpT"))),B=l(a("MigZ")),F=l(a("y0tt")),I=(a("kLkQ"),a("jBtN")),P=60,T=(v=b["default"].create(),v((w=function(e){function t(){var e;return(0,m["default"])(this,t),e=(0,g["default"])(this,(0,h["default"])(t).call(this)),e.handleRegisterKey=function(t){13===t.which&&e.handleSubmit()},e.settime=function(){e.setState({showTime:!0},function(){if(0===P)return e.setState({showTime:!1,getCodeText:"\u91cd\u65b0\u83b7\u53d6"}),P=60,!1;P--,e.setState({time:P}),e.registerTimer=setTimeout(function(){e.settime()},1e3)})},e.getRegistCode=function(){var t=(0,y["default"])(e),a=e.props.form,l=a.validateFields;a.getFieldsValue,a.getFieldValue;l(["phone"],function(e,a){e||(0,F["default"])(B["default"].register.getRegistCode.api()+"?phone=".concat(a.phone),{method:B["default"].register.getRegistCode.type}).then(function(e){e&&0===e.code?(t.settime(),f["default"].success(e.message)):e&&f["default"].error(e.message)})})},e.handleSubmit=function(t){var a=(0,y["default"])(e);t&&t.stopPropagation();var l=e.props.form,r=l.validateFields,n=l.getFieldsValue,d=(l.getFieldValue,n());if(d.agreement){var s=d.code,o=d.password,u=d.rePassWord;if(o===u)if(d.code){var i=e.props;i.dispatch,i.type;r(function(e,t){e||(0,F["default"])(B["default"].register.registCode.api()+"?code=".concat(s,"&account=").concat(d.phone,"&type=0"),{method:B["default"].register.registCode.type}).then(function(e){0===e.code?(delete t.rePassWord,delete t.agreement,delete t.code,(0,F["default"])(B["default"].register.regist.api()+"?".concat(I.utils.queryString(t)),{method:B["default"].register.regist.type}).then(function(e){e&&(0===e.code?(f["default"].success(e.message),sessionStorage.originPath=location.pathname,a.props.history.push("/perLogin")):f["default"].error(e.message))})):f["default"].error(e.message)})})}else r(["code"]);else f["default"].warning("\u4e24\u6b21\u5bc6\u7801\u8f93\u5165\u4e0d\u4e00\u81f4")}else f["default"].warning("\u6ce8\u518c\u65f6\u8bf7\u5148\u9605\u8bfb\u5e76\u540c\u610f\u300a\u91d1\u5bc6\u5de5\u4e1a\u4e91\u7528\u6237\u6ce8\u518c\u534f\u8bae\u300b")},e.handlePhoneChange=function(t){var a=/^1[3|4|5|6|7|8|9][0-9]{9}$/,l=e.state.getCode;a.test(t.target.value)?e.setState({getCode:!0}):l&&e.setState({getCode:!1})},e.state={showTime:!1,time:null,getCode:!1},e.registerTimer=null,e}return(0,E["default"])(t,e),(0,p["default"])(t,[{key:"componentDidMount",value:function(){document.addEventListener("keydown",this.handleRegisterKey.bind(this))}},{key:"componentWillUnmount",value:function(){clearTimeout(this.registerTimer),document.removeEventListener("keydown",this.handleRegisterKey)}},{key:"render",value:function(){var e=this.props.form.getFieldDecorator,t=this.state,a=t.showTime,l=t.time,r=t.getCode,f=c["default"].createFromIconfontCN({scriptUrl:"//at.alicdn.com/t/font_1093886_8rqycotthom.js"});c["default"].createFromIconfontCN({scriptUrl:"//at.alicdn.com/t/font_1093886_6s5qjyg1qs.js"});return k["default"].createElement("div",{className:"registerWrap"},k["default"].createElement("div",{className:"content"},k["default"].createElement("div",{className:"registerBox"},k["default"].createElement("div",{className:"leftBox_wrap"},k["default"].createElement("h2",null,"\u4e3a\u5de5\u4e1a\u8d4b\u80fd\uff0c\u4e0e\u4f19\u4f34\u5171\u751f"),k["default"].createElement("p",null,"\u7acb\u8db3\u6570\u5b57\u5316\u8f6c\u578b\uff0c\u52a9\u529b\u4f01\u4e1a\u9ad8\u6548\u8fd0\u8425")),k["default"].createElement("div",{className:"form"},k["default"].createElement("h3",null,"\u7528\u6237\u6ce8\u518c"),k["default"].createElement(b["default"],null,k["default"].createElement(b["default"].Item,null,e("phone",{rules:[{required:!0,message:"\u624b\u673a\u53f7\u7801\u683c\u5f0f\u4e0d\u6b63\u786e\uff01",pattern:/^1[3|4|5|6|7|8|9][0-9]{9}$/}]})(k["default"].createElement(i["default"],{placeholder:"\u8bf7\u8f93\u5165\u624b\u673a\u53f7\u7801",onChange:this.handlePhoneChange,addonBefore:k["default"].createElement(f,{type:"icon-shouji"})}))),k["default"].createElement(b["default"].Item,null,k["default"].createElement(s["default"],{className:"row"},k["default"].createElement(o["default"],{span:24},e("code",{rules:[{required:!0,message:"\u8bf7\u586b\u5165\u9a8c\u8bc1\u7801"}]})(k["default"].createElement(i["default"],{placeholder:"\u8bf7\u586b\u5165\u9a8c\u8bc1\u7801",addonBefore:k["default"].createElement(c["default"],{theme:"filled",type:"safety-certificate"}),addonAfter:a?k["default"].createElement("span",{className:"btnClass",style:{color:"rgba(255,255,255,1)",opacity:.5}},k["default"].createElement(u["default"],{type:"vertical"}),l,"s"):k["default"].createElement("span",{className:"btnClass",style:r?{color:"rgba(255,206,66,1)"}:{color:"rgba(255,255,255,1)",opacity:.5},onClick:r?this.getRegistCode:function(){}},k["default"].createElement(u["default"],{type:"vertical"}),"\u83b7\u53d6\u9a8c\u8bc1\u7801")}))))),k["default"].createElement(b["default"].Item,null,e("name",{rules:[{required:!0,message:"\u8bf7\u586b\u5165\u7528\u6237\u540d\uff01"}]})(k["default"].createElement(i["default"],{placeholder:"\u8bf7\u586b\u5165\u7528\u6237\u540d",addonBefore:k["default"].createElement(f,{type:"icon-user"})}))),k["default"].createElement(b["default"].Item,null,e("password",{rules:[{required:!0,message:"\u8bf7\u586b\u5165\u5bc6\u7801,\u5bc6\u7801\u4e3a\u5b57\u6bcd\u548c\u6570\u5b57\u7684\u7ec4\u5408\u4e14\u4e0d\u5c11\u4e8e8\u4f4d",pattern:/^(?![0-9]+$)(?![a-zA-Z]+$)[0-9a-zA-Z]{8,}/}]})(k["default"].createElement(i["default"],{type:"password",placeholder:"\u8bf7\u586b\u5165\u5bc6\u7801",addonBefore:k["default"].createElement(c["default"],{theme:"filled",type:"lock"})}))),k["default"].createElement(b["default"].Item,null,e("rePassWord",{rules:[{required:!0,message:"\u8bf7\u518d\u6b21\u8f93\u5165\u5bc6\u7801,\u5bc6\u7801\u4e3a\u5b57\u6bcd\u548c\u6570\u5b57\u7684\u7ec4\u5408\u4e14\u4e0d\u5c11\u4e8e8\u4f4d",whitespace:!0}]})(k["default"].createElement(i["default"],{placeholder:"\u8bf7\u518d\u6b21\u8f93\u5165\u5bc6\u7801",type:"password",addonBefore:k["default"].createElement(c["default"],{theme:"filled",type:"lock"})}))),k["default"].createElement(b["default"].Item,{style:{textAlign:"center"}},e("agreement",{valuePropName:"checked"})(k["default"].createElement(d["default"],{className:"radio"},"\u540c\u610f\u5e76\u9605\u8bfb",k["default"].createElement(N["default"],{to:"/agreement/regment",style:{color:"rgba(252, 187, 1, 1)"}},"\u300a\u91d1\u5bc6\u5de5\u4e1a\u4e91\u7528\u6237\u6ce8\u518c\u534f\u8bae\u300b")))),k["default"].createElement(b["default"].Item,{style:{textAlign:"center"}},k["default"].createElement(n["default"],{type:"primary",className:"registerBtn",onClick:this.handleSubmit},"\u7acb \u5373 \u6ce8 \u518c")))))))}}]),t}(k.Component),C=w))||C);t["default"]=T},bIAp:function(e,t,a){}}]);