import React, { Component } from 'react'
import '../../common/styles/layout/header.less'
import '../../common/styles/theme/headFoot.less'
import withRouter from 'umi/withRouter'
import { utils, go } from 'common'
import { connect } from 'dva'
import { changeEnterprise } from 'services'
import router from 'umi/router'
import { message } from 'antd'

const { trade, con, tms } = go
const { imgGet, openModal } = utils

//传入参数：home、supply、ifms、transport、erp、news,ERP（对应的是相关功能的页面、其子页面传入同样的参数）
//参数类型：字符串

@withRouter
@connect(({ global, loading }) => ({
  global
}))
export default class Header extends Component {
  constructor(props) {
    super(props)
    this.state = {
      now: null,
      currentCompany: 1,
      currentNav: 'home',
      navHover: '',
      navList: [
        {
          key: 'home',
          navTitle: '首页',
          link: '/'
        },
        {
          key: 'produceService',
          navTitle: '产品服务',
          link: '/',
          noClick: true,
          productList: [
            {
              key: 'brain',
              navTitle: '蜜云智造',
              link: '/brain'
            },
            {
              key: 'supplyChain',
              navTitle: '领蜂供应链',
              link: '/supplyChain'
            },
            {
              key: 'crm',
              navTitle: '蜂客CRM',
              link: '/crm'
            },
            {
              key: 'erp',
              navTitle: '蜂巢进销存',
              link: '/erp'
            },

            {
              key: 'shopkeeper',
              navTitle: '蜂掌柜',
              link: '/shopkeeper'
            },

            {
              key: 'factory',
              navTitle: '工厂运营',
              link: '/factory'
            },
            {
              key: 'transport',
              navTitle: '集蜂联运',
              link: '/transport'
            },
            {
              key: 'trade',
              navTitle: '线上蜂贸',
              link: '/trade'
            }
          ]
        },
        {
          key: 'solution',
          navTitle: '解决方案',
          link: '/Solution',
          noClick: true,
          productList: [
            {
              key: 'energy',
              navTitle: '能源管控系统',
              link: '/solutions/energy'
            },
            {
              key: 'agriculture',
              navTitle: '智慧园区农业系统',
              link: '/solutions/agriculture'
            },
            {
              key: 'location',
              navTitle: '高精准定位系统',
              link: '/solutions/location'
            },
            {
              key: 'environmental',
              navTitle: '环境监测系统',
              link: '/solutions/environment'
            },
            {
              key: 'Intelligent',
              navTitle: '智能过磅系统',
              link: '/solutions/balance'
            },
            {
              key: 'Construction',
              navTitle: '智慧工地系统',
              link: '/solutions/smartSite'
            }
          ]
        },
        {
          key: 'news',
          navTitle: '资讯中心',
          link: '/news'
        },
        {
          key: 'aboutUs',
          navTitle: '关于我们',
          link: '/aboutUs'
        }
      ]
    }
    this.pathList = [
      'aboutUs',
      'brain',
      'supplyChain',
      'crm',
      'erp',
      'agreement'
    ]
  }
  componentDidMount() {
    this.getTime()
  }

  componentWillReceiveProps(props, state) {
    const path = props.location.pathname
    this.pathList.forEach(item => {
      if (path.indexOf(item) !== -1) {
        this.setState({
          currentNav: item
        })
      }
    })

    // if (props.location.pathname === '/aboutUs') {
    //   this.setState({
    //     currentNav: 'aboutUs'
    //   })
    // }
  }

  componentDidUpdate() {}

  //获取当天小时数
  getTime = () => {
    const {
      global: { user }
    } = this.props
    const now = new Date().getHours()
    this.setState({
      now,
      currentCompany: user.orgId
    })
  }
  onMouseOVerNav = item => {
    this.setState({
      navHover: item
    })
  }
  onMouseLeaveNav = item => {
    this.setState({
      navHover: ''
    })
  }
  clickItem = item => {
    if (item.productList && item.productList.length > 0) {
      this.setState({
        currentNav: item.key
      })
    } else {
      router.push(`${item.link}`)
      window.scrollTo(0, 0)
      this.setState({
        currentNav: item.key
      })
    }
  }
  changeCompany = id => {
    const { dispatch } = this.props
    changeEnterprise(id).then(res => {
      if (res.code === 0) {
        dispatch({
          type: 'global/getSelfInfo',
          payload: {
            sysToken: localStorage.sysToken
          },
          callback: (code, msg, data) => {
            if (code === 0) {
              this.setState({
                currentCompany: id
              })
            } else {
              // message.error(msg);
            }
          }
        })
      }
    })
  }

  goPersonal = () => {
    router.push('/personal')
  }
  getUserInfo = () => {
    const { dispatch } = this.props
    dispatch({
      type: 'global/getSelfInfo',
      payload: {
        sysToken: localStorage.sysToken
      },
      callback: (code, msg, data) => {
        if (code === 0) {
        } else {
          // message.error(msg);
        }
      }
    })
  }
  quit = sysToken => {
    const { dispatch } = this.props
    dispatch({
      type: 'global/userOut',
      payload: {
        sysToken: sysToken
      },
      callback: (code, msg, data) => {
        if (code === 0) {
          this.getTime()
          this.getUserInfo()
          sessionStorage.removeItem('loginSuccessNextPage')
          sessionStorage.originPath = location.pathname
          router.push('/perLogin')
        } else {
          // message.error(msg);
          dispatch({
            type: 'global/reSetUser',
            payload: {}
          })
        }
      }
    })
  }

  handleOA = () => {
    message.warning('请打开ie浏览器访问OA系统')
    window.open('http://yyoa.scmdjt.com:6666/')
  }

  turnTo = (link, title) => {
    if (title === '询价信息' || title === '现货信息') {
      trade(link)
      localStorage.setItem('tradeType', link)
    } else {
      if (link === '/perLogin') {
        this.setState({
          navHover: link,
          currentNav: link
        })
        let time = new Date()
        let originPathTime = time.getTime()
        sessionStorage.originPath =
          this.props.location.pathname + this.props.location.search
        sessionStorage.originPathTime = originPathTime
        router.push(`${link}`)
      } else {
        router.push(`${link}`)
      }
    }
    window.scrollTo(0, 0)
  }

  navLink(link, item) {
    this.setState(
      {
        navHover: link,
        currentNav: link
      },
      () => {
        if (item) {
          this.clickItem(item)
        }
      }
    )
    router.push(`${link}`)
    window.scrollTo(0, 0)
  }
  render() {
    const { now, currentNav, navHover, currentCompany, navList } = this.state
    const {
      location: { pathname },
      global: { user, company, authenticatedList }
    } = this.props

    return (
      <div className="flex-column headerWrap">
        <div className="flex-column headerCont">
          <div className="flex-row headerContIner">
            <div className="headerLogo">
              <a
                className="logoSize"
                onClick={() => {
                  this.navLink('/', {
                    key: 'home',
                    navTitle: '首页',
                    link: '/'
                  })
                }}
              >
                <img src={imgGet('headerLog', 'header')} />
              </a>
            </div>
            <div className="flex-row navWrap">
              {navList && navList.length > 0
                ? navList.map((navItem, navIndex) => {
                    return (
                      <div
                        className={`navItem ${
                          navItem.key === currentNav ? 'navActive' : ''
                        } ${navItem.key === navHover ? 'navItemhover' : ''}`}
                        key={`${navItem.key} + 'nav' + ${navIndex}`}
                        onClick={
                          navItem.noClick
                            ? () => {}
                            : this.clickItem.bind(this, navItem)
                        }
                        onMouseOver={this.onMouseOVerNav.bind(
                          this,
                          navItem.key
                        )}
                        onMouseLeave={this.onMouseLeaveNav.bind(
                          this,
                          navItem.key
                        )}
                      >
                        <span className="navItemText">{navItem.navTitle}</span>

                        {navItem.productList &&
                        navItem.productList.length > 0 ? (
                          <div className={`subNavWrap`}>
                            {navItem.productList.map(
                              (subNavItem, subNavItemIndex) => {
                                return (
                                  <a
                                    className={
                                      navItem.key === 'solution'
                                        ? 'subItem subItemSolution'
                                        : 'subItem'
                                    }
                                    key={`${subNavItem.key} + 'subNav' + ${subNavItemIndex}`}
                                    onClick={this.navLink.bind(
                                      this,
                                      subNavItem.link,
                                      subNavItem
                                    )}
                                  >
                                    {subNavItem.navTitle}
                                  </a>
                                )
                              }
                            )}
                          </div>
                        ) : null}
                      </div>
                    )
                  })
                : null}
            </div>
            <div className="flex-row navRight">
              <div className="weiXinCode">
                <cite>微信公众号</cite>
                <div className="code">
                  <img className="codeImg" src={imgGet('code', 'footer')} />
                </div>
              </div>

              {user.id ? (
                <div className="loginedName">
                  {user.head ? (
                    <img className="loginedLog" src={user.head} />
                  ) : (
                    <img
                      className="loginedLog"
                      src={imgGet('user', 'header')}
                    />
                  )}

                  {user.name ? user.name : '用户名'}
                  <img className="down" src={imgGet('dropDown', 'header')} />
                  <img className="up" src={imgGet('up', 'header')} />
                  <ul className="loginOut">
                    <li>
                      <span>{user.name ? user.name : '用户名'}</span>
                      {user.org_name ? (
                        <span className="haveCompany">
                          ★&nbsp;&nbsp;企业认证
                        </span>
                      ) : authenticatedList.length > 0 ? (
                        <span className="noCompany" onClick={this.goPersonal}>
                          认证中
                        </span>
                      ) : (
                        <span className="noCompany" onClick={this.goPersonal}>
                          未认证
                        </span>
                      )}
                    </li>
                    <li>
                      <span
                        className="shenlue"
                        title={user.org_name ? user.org_name : '暂无'}
                      >
                        {user.org_name ? user.org_name : '暂无'}
                      </span>
                      <span
                        className="userPost"
                        title={user.post ? user.post : '暂无'}
                      >
                        {user.post ? user.post : '暂无'}
                      </span>
                    </li>
                    {/* <li onClick={this.goPersonal}>
                        <span>个人中心</span>
                        <span className="rightArrow}>></span>
                      </li> */}
                    {/* <li>
                      <span>消息</span>
                      <span>
                        <span className="msgNum}>5</span>
                        <span className="rightArrow}>></span>
                      </span>
                    </li> */}
                    <li>
                      <span>{/* 工单 */}</span>
                      <span
                        className="quit"
                        onClick={this.quit.bind(this, user.sysToken)}
                      >
                        退出
                      </span>
                    </li>
                  </ul>
                </div>
              ) : (
                <div className="noLoginWrap">
                  <a
                    className="noLoginText"
                    onClick={this.turnTo.bind(this, '/perLogin')}
                  >
                    登录
                  </a>
                  <a
                    className="noLoginText registerPL"
                    onClick={() => {
                      this.navLink('/register')
                    }}
                  >
                    注册
                  </a>
                </div>
              )}
              <div className="btnConsole" onClick={con}>
                <a className="consoleText">控制台</a>
              </div>
            </div>
          </div>
        </div>
      </div>
    )
  }
}
const active = {
  display: 'inline-block',
  color: 'eca702',
  borderBottom: '2px solid #eca702'
}
const activeOther = {
  display: 'inline-block',
  height: '30px',
  color: '#fff',
  borderBottom: '2px solid #fff',
  fontWeight: '600'
}
