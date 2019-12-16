import { Component } from 'react'
import { Layout, Modal, message, Spin, LocaleProvider } from 'antd'
import zhCN from 'antd/lib/locale-provider/zh_CN'
import withRouter from 'umi/withRouter'
import { utils } from 'common'
import router from 'umi/router'
import { connect } from 'dva'
import { Header, Footer } from './components'
import { SideBar, ContactModal } from '../components'

const { FuckIE } = utils
const { Content } = Layout
const confirm = Modal.confirm

@connect(({ global, loading }) => ({
  global
}))
class BasicLayout extends Component {
  constructor(props) {
    super(props)
    this.state = {
      collapsed: false,
      logining: false,
      pathName: null,
      visible: false
    }
  }
  componentDidMount() {
    const { dispatch, global } = this.props
    if (FuckIE()) {
      document.getElementById('IEModal').style.display = 'block'
    }
    if (localStorage.sysToken) {
      dispatch({
        type: 'global/getEnterprises',
        payload: {
          sysToken: localStorage.sysToken
        },
        callback: (code, msg, data) => {
          if (code === 0) {
            if (data.length >= 1) {
              this.getUserInfo(true)
            } else {
              this.getUserInfo(false)
            }
          } else {
            // message.error(msg);
          }
        }
      })
    }
  }
  handleMenuCollapse = () => {
    this.setState({
      collapsed: !this.state.collapsed
    })
  }
  getUserInfo = ishave => {
    const { dispatch } = this.props
    if (localStorage.sysToken) {
      dispatch({
        type: 'global/getSelfInfo',
        payload: {
          sysToken: localStorage.sysToken
        },
        callback: (code, msg, data) => {
          if (code === 0) {
            if (this.props.location.pathname === '/perLogin') {
              router.replace('/')
            }
            if (!ishave) {
              dispatch({
                type: 'global/getPersonAuthenticatedList',
                payload: {
                  phone: data.phone
                },
                callback: (code, msg, data) => {
                  if (code === 0) {
                  } else {
                    // message.error(msg);
                  }
                }
              })
            }
          } else {
            // message.error(msg);
          }
        }
      })
    }
  }
  onMenuClick(e, a) {
    const { dispatch } = this.props
    switch (e.key) {
      case 'logout':
        confirm({
          title: '退出登录',
          content: '确定退出供应链工作台吗?',
          onOk() {
            dispatch({
              type: 'global/logout',
              payload: {
                uuid: sessionStorage.financeToken
              },
              callback: msg => message.error(msg)
            })
          }
        })

        break

      default:
        break
    }
  }

  render() {
    const {
      children,
      location: { pathname },
      global: { user, menus }
    } = this.props
    const { collapsed, logining } = this.state
    const noFooter = ['getBack', 'register', 'perLogin']
    return pathname === '/' ? (
      <LocaleProvider locale={zhCN}>
        <Layout>
          <Header />
          <Content>{children}</Content>
          <ContactModal visible={this.props.global.visible} />
          <SideBar pathName="home" />
          <Footer />
        </Layout>
      </LocaleProvider>
    ) : (
      <LocaleProvider locale={zhCN}>
        <Layout>
          <Header style={{ padding: 0 }} />
          <Content>{children}</Content>
          <ContactModal visible={this.props.global.visible} />
          <SideBar
            pathName={pathname === '/' ? 'home' : pathname.split('/')[1]}
          />
          {noFooter.some(item => {
            return pathname.indexOf(item) !== -1
          }) ? null : (
            <Footer />
          )}
        </Layout>
      </LocaleProvider>
    )
  }
}

export default withRouter(BasicLayout)
