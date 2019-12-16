import { Component } from 'react';
import { Layout, Icon, message, LocaleProvider } from 'antd';
import SiderMenu from "../components/SiderMenu/SiderMenu";
import { getMenuData } from '../common/menu';
import GlobalHeader from "../components/GlobalHeader";
import zh_CN from 'antd/lib/locale-provider/zh_CN';
import router from 'umi/router';

const { Content, Header, Footer } = Layout;

class BasicLayout extends Component {
  constructor(props) {
    super(props);
    this.state = {
      collapsed: false
    };
  }

  handleMenuCollapse = () => {
    this.setState({
      collapsed: !this.state.collapsed,
    });
  };

  render() {
    const { children, location } = this.props;
    const { collapsed } = this.state;
    if (window.location.href.indexOf('http://') > -1 && window.location.href.indexOf('http://1') === -1 && window.location.href.indexOf('http://localhost') === -1) {
      window.location.href = window.location.href.indexOf('?') > -1 ? window.location.href.replace('http://', 'https://') + '&v=1.0' : window.location.href.replace('http://', 'https://') + '?v=1.0';
    }
    else if (!localStorage.version || localStorage.version !== '1.0') {
      localStorage.setItem('version', '1.0');
      window.location.href = window.location.href.indexOf('?') > -1 ? window.location.href + '&v=1.0' : window.location.href + '?v=1.0'
    }
    if (location.pathname !== '/login' && !sessionStorage.sysToken) {
      router.push('/login');
      return ''
    }
    if( location.pathname === '/login' ) {
      return (
        <LocaleProvider locale={zh_CN}>
          { children }
        </LocaleProvider>
      )
    } else {
      return (
        <LocaleProvider locale={zh_CN}>
          <Layout>
            <SiderMenu
              collapsed={collapsed}
              menuData={getMenuData()}
              location={location}
              onCollapse={this.handleMenuCollapse}
            />
            <Layout>
              <Header style={{ padding: 0 }}>
                <GlobalHeader
                  collapsed={collapsed}
                  currentUser={{
                    name: 'Serati Ma',
                    avatar: 'https://gw.alipayobjects.com/zos/rmsportal/BiazfanxmamNRoxxVxka.png',
                    userid: '00000001',
                    notifyCount: 12,
                  }}
                  onCollapse={this.handleMenuCollapse}
                />
              </Header>
              <Content style={{ height: '100%' }}>
                { children }
              </Content>
            </Layout>
          </Layout>
        </LocaleProvider>
      );
    }
  }
}

export default BasicLayout;
