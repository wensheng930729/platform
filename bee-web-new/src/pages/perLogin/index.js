import React, { Component } from "react";
import "common/styles/perLogin/index.less";
import { Form, Icon, Input, Button, message } from "antd";
import { utils, go } from "common";
import Link from "umi/link";
import withRouter from "umi/withRouter";
import router from "umi/router";
import { connect } from "dva";
import { getLoginCode, changeEnterprise } from "services";

const { clearUserInfo } = utils;

const { con, tms, supplychain, trade, ERP, OA, ifms } = go;
const { Item } = Form;

@withRouter
@connect(({ global }) => ({
  global
}))
class index extends Component {
  constructor(props) {
    super(props);
    this.state = {
      isShow: true,
      showTime: false,
      phoneChange: false,
      getCodeText: "获取验证码",
      time: null,
      isPhone: false
    };
    this.loginSuccessNextPage = false;
    this.loginTimer = null;
    this.isLoginOut = this.props.location.query.isLoginOut;
  }
  componentDidMount() {
    if (this.isLoginOut) {
      localStorage.clear();
      sessionStorage.clear();
      router.push("/perLogin");
    } else {
      if (localStorage.sysToken && this.props.global.login) {
        //登录状态中跳入登录页面
        router.replace("/");
      }
    }
    document.addEventListener("keydown", this.handleEnterKey.bind(this));
  }
  componentWillUnmount() {
    clearTimeout(this.loginTimer);
    document.removeEventListener("keydown", this.handleEnterKey);
  }
  handleEnterKey = e => {
    const { isShow } = this.state;
    if (e.which === 13) {
      if (isShow) {
        this.login("login");
      } else {
        this.login("loginCode");
      }
    }
  };
  clickItem = show => {
    this.setState({
      isShow: show
    });
  };
  timeOut = second => {
    second = second - 1;
    this.setState({
      time: second
    });
  };
  runtime = () => {
    this.setState(
      {
        showTime: true
      },
      () => {
        if (countdown === 0) {
          this.setState({
            showTime: false,
            getCodeText: "重新获取"
          });
          countdown = 60;
          return false;
        } else {
          countdown--;
          this.setState({
            time: countdown
          });
        }
        this.loginTimer = setTimeout(() => {
          this.runtime();
        }, 1000);
      }
    );
  };

  settime = () => {
    let phone = this.props.form.getFieldValue("username");
    let reg = /^1[3|4|5|6|7|8|9][0-9]{9}$/;
    let phoneNumber = phone.match(reg) && phone.match(reg)[0];
    if (phoneNumber && countdown === 60) {
      getLoginCode({ phone: phoneNumber }).then(res => {
        if (res) {
          if (res.code === 0) {
            message.success(res.message);
            this.runtime();
          } else {
            message.error(res.message);
          }
        }
      });
    }
  };
  getUserInfo = (ishave, token) => {
    const { dispatch } = this.props;
    let turnToPath = false;
    if (sessionStorage.originPath) {
      turnToPath =
        sessionStorage.originPath.indexOf("perLogin") !== -1 ||
        sessionStorage.originPath.indexOf("getBack") !== -1 ||
        sessionStorage.originPath.indexOf("register") !== -1;
    }
    if (localStorage.sysToken) {
      dispatch({
        type: "global/getSelfInfo",
        payload: {
          sysToken: localStorage.sysToken
        },
        callback: (code, msg, data) => {
          if (code === 0) {
            if (!ishave) {
              // dispatch({
              //   type: "global/getPersonAuthenticatedList",
              //   payload: {
              //     phone: data.phone
              //   },
              //   callback: (code, msg, data) => {
              //     if (code === 0) {
              //     } else {
              //       message.error(msg);
              //     }
              //   }
              // });
              if (
                sessionStorage.originPath === undefined ||
                (sessionStorage.originPath && turnToPath)
              ) {
                router.push("/");
              } else {
                router.push(sessionStorage.originPath);
              }
            } else {
              let whoFirst = true;
              if (
                sessionStorage.loginSuccessNextPageTime &&
                sessionStorage.originPathTime
              ) {
                whoFirst =
                  Number(sessionStorage.loginSuccessNextPageTime) >
                  Number(sessionStorage.originPathTime);
              }
              if (sessionStorage.loginSuccessNextPage && whoFirst) {
                this.nextPage(token);
              } else {
                if (
                  sessionStorage.originPath === undefined ||
                  (sessionStorage.originPath && turnToPath)
                ) {
                  router.push("/");
                } else {
                  router.push(sessionStorage.originPath);
                }
              }
            }
          } else {
            // message.error(msg);
          }
        }
      });
    }
  };
  nextPage = token => {
    const turnPath = sessionStorage.loginSuccessNextPage;
    const tradeType = localStorage.tradeType;
    sessionStorage.removeItem("loginSuccessNextPage");
    localStorage.removeItem("tradeType");
    if (turnPath) {
      if (turnPath === "con") {
        con(token);
      } else if (turnPath === "tms") {
        tms();
        router.push("/");
      } else if (turnPath === "supplychain") {
        supplychain();
        router.push("/");
      } else if (turnPath === "trade") {
        if (tradeType) {
          trade(tradeType);
        } else {
          trade();
        }
        router.push("/");
      } else if (turnPath === "ERP") {
        ERP();
      } else if (turnPath === "OA") {
        OA();
        router.push("/");
      } else if (turnPath === "ifms") {
        ifms();
      } else {
        router.push("/");
      }
    }

    // localStorage.removeItem("loginSuccessNextPage");
  };
  getUserCompanyInfo = (orgId, token) => {
    const { dispatch } = this.props;
    if (localStorage.sysToken) {
      dispatch({
        type: "global/getEnterprises",
        payload: {
          sysToken: localStorage.sysToken
        },
        callback: (code, msg, data) => {
          if (code === 0) {
            if (data.length >= 1) {
              let companyId = orgId || data[0].id;
              changeEnterprise(companyId).then(res => {
                if (res.code === 0) {
                  this.getUserInfo(true, token);
                }
              });
            } else {
              this.getUserInfo(false, token);
              router.push("/");
            }
          } else {
            // message.error(msg);
          }
        }
      });
    }
  };
  login = type => {
    this.props.form.validateFields((err, values) => {
      if (!err && values.username) {
        const { dispatch } = this.props;
        if (type === "login") {
          dispatch({
            type: "global/login",
            payload: {
              userType: 0,
              ...values
            },
            callback: (code, msg, data) => {
              if (code === 0) {
                setTimeout(() => {
                  this.getUserCompanyInfo(data.orgId, data.sysToken);
                }, 1000);
              } else {
                message.error(msg);
              }
            }
          });
        } else {
          dispatch({
            type: "global/loginCode",
            payload: {
              ...values
            },
            callback: (code, msg, data) => {
              if (code === 0) {
                setTimeout(() => {
                  this.getUserCompanyInfo(data.orgId, data.sysToken);
                }, 1000);
              } else {
                message.error(msg);
              }
            }
          });
        }
      }
    });
  };
  getPhoneNumber = e => {
    let reg = /^1[3|4|5|6|7|8|9][0-9]{9}$/;
    let isPhone = e.target.value.match(reg) && e.target.value.match(reg)[0];
    if (isPhone) {
      this.setState({
        isPhone: true
      });
    } else {
      this.setState({
        isPhone: false
      });
    }
  };
  componentWillReceiveProps() {
    let phone = this.props.form.getFieldValue("username");
    let reg = /^1[3|4|5|6|7|8|9][0-9]{9}$/;
    let isPhone = phone && phone.match(reg) && phone.match(reg)[0];
    if (isPhone) {
      this.setState({
        isPhone: true
      });
    } else {
      this.setState({
        isPhone: false
      });
    }
  }
  render() {
    const {
      isShow,
      showTime,
      time,
      getCodeText,
      phoneChange,
      isPhone
    } = this.state;
    const { getFieldDecorator } = this.props.form;
    return (
      <div className="loginWrap">
        <div className="loginSlogan">
          <div className="loginSloganTitle">为工业赋能，与伙伴共生</div>
          <div className="loginSloganContent">
            立足数字化转型，助力企业高效运营
          </div>
        </div>
        <div className="loginBox">
          <div className="loginContent">
            <ul className="loginNav">
              <li
                onClick={this.clickItem.bind(this, true)}
                className={isShow ? "loginActive" : "loginNormal"}
              >
                密码登录
              </li>
              <li
                onClick={this.clickItem.bind(this, false)}
                className={isShow ? "loginNormal" : "loginActive"}
              >
                短信登录
              </li>
            </ul>
            {isShow ? (
              <div className="codeLogin">
                <Form className="loginForm">
                  {/* <h3>账号</h3> */}
                  <Item className="loginInputBox">
                    {getFieldDecorator("username", {
                      rules: [
                        { required: true, message: "请输入手机号码/用户名" }
                      ]
                    })(
                      <Input
                        placeholder="用户名"
                        style={{
                          background: "rgba(0,0,0,0)"
                        }}
                        prefix={<img src={require("assets/login/user.png")} />}
                      />
                    )}
                  </Item>
                  {/* <h3>密码</h3> */}
                  <Item className="loginInputBox">
                    {getFieldDecorator("password", {
                      rules: [
                        {
                          required: true,
                          message: "请填入密码"
                          // pattern: /^(?![0-9]+$)(?![a-zA-Z]+$)[0-9a-zA-Z]{8,}/
                        }
                      ]
                    })(
                      <Input
                        type="password"
                        placeholder="密码"
                        prefix={
                          <img src={require("assets/login/password.png")} />
                        }
                      />
                    )}
                  </Item>
                  <Item>
                    <div className="loginForgetCode">
                      <Link to="/register">注册</Link>
                      <Link to="/getBack">忘记密码？</Link>
                    </div>
                    <Button
                      className="loginBtn"
                      type="primary"
                      onClick={this.login.bind(this, "login")}
                    >
                      登录
                    </Button>
                  </Item>
                </Form>
              </div>
            ) : (
              <div className="phoneLogin">
                <Form onSubmit={this.handleSubmit} className="loginForm">
                  {/* <h3>账号</h3> */}
                  <Item className="loginInputBox">
                    {getFieldDecorator("username", {
                      rules: [
                        {
                          required: true,
                          message: "手机号码格式不正确！",
                          pattern: /^1[3|4|5|6|7|8|9][0-9]{9}$/
                        }
                      ]
                    })(
                      <Input
                        placeholder="手机号码"
                        onChange={this.getPhoneNumber}
                        ref={inputPhone => (this.inputPhone = inputPhone)}
                        prefix={<img src={require("assets/login/phone.png")} />}
                      />
                    )}
                  </Item>
                  {/* <h3>验证码</h3> */}
                  <Item className="loginInputBox">
                    {getFieldDecorator("code", {
                      rules: [{ required: true, message: "请填入验证码" }]
                    })(
                      <Input
                        placeholder="验证码"
                        prefix={<img src={require("assets/login/code.png")} />}
                        className="loginInputCode"
                        suffix={
                          <div className="loginGetCode">
                            <span className="loginLine">|</span>
                            <span>
                              {showTime ? (
                                <span
                                  className="loginNormalBtnClass"
                                  disabled={showTime}
                                >
                                  {time}s
                                </span>
                              ) : (
                                <span
                                  className={
                                    isPhone
                                      ? "loginBtnClass"
                                      : "loginNormalBtnClass"
                                  }
                                  onClick={isPhone ? this.settime : null}
                                >
                                  {getCodeText}
                                </span>
                              )}
                            </span>
                          </div>
                        }
                      />
                    )}
                  </Item>
                  <Item>
                    <div className="loginForgetCode">
                      <Link to="/register">注册</Link>
                    </div>
                    <Button
                      className="loginBtn"
                      type="primary"
                      onClick={this.login.bind(this, "loginCode")}
                    >
                      登录
                    </Button>
                  </Item>
                </Form>
              </div>
            )}
          </div>
        </div>
      </div>
    );
  }
}

let countdown = 60;
const WrappedNormalLoginForm = Form.create({})(index);

export default WrappedNormalLoginForm;
