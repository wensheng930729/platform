import { Component } from 'react';
import { Icon, Form, Input, Button, message } from 'antd';
import styles from './index.less';
import loginImage from '../../assets/login.png';
import { domainUrl, path } from '../../utils/api';
import { post, fetchPost } from '../../utils/fetch';
import router from 'umi/router';

const FormItem = Form.Item;

@Form.create()
export default class index extends Component {
  constructor(props) {
    super(props);
    this.state = {
      loading: false
    }
  }

  login = () => {
    const { form } = this.props;
    form.validateFields((err, data) => {
      if (err) {
        return;
      }
      let formBody = [];
      let datas = { ...data }
      for (var name in datas) {
        formBody.push(name + "=" + datas[name]);
      }
      formBody = formBody.join("&");
      fetchPost(path.login, domainUrl, formBody, "login")
        .then(res => {
          if (res.code === 1 && res.sysToken) {
            sessionStorage.setItem("sysToken", res.sysToken);
            router.push('enterprise')
          } else {
            message.error("登录失败")
          }
        })
    })
  }

  render() {
    const { getFieldDecorator } = this.props.form;
    const { loading } = this.state;
    let height = document.body.clientHeight;
    return (
      <div className={styles.login} style={{ height: height }}>
        <div className={styles.container}>
          <div className={styles.loginLeft}>
            <div className={styles.loginTitle}>
              <p>金蜜工业卫士后台管理</p>
              <p>Backend Management</p>
            </div>
            <Form hideRequiredMark={true}>
              <FormItem>
                {getFieldDecorator('username', {
                  rules: [{
                    required: true, message: '请输入账号',
                  }],
                })(
                  <Input
                    style={{ height: 42 }}
                    type="text"
                    placeholder="账号"
                    prefix={<Icon type="user" style={{ color: 'rgba(0,0,0,.25)' }} />}
                  />
                )}
              </FormItem>
              <FormItem>
                {getFieldDecorator('password', {
                  rules: [{
                    required: true, message: '请输入密码',
                  }],
                })(
                  <Input
                    style={{ height: 42 }}
                    type="password"
                    placeholder="密码"
                    prefix={<Icon type="lock" style={{ color: 'rgba(0,0,0,.25)' }} />}
                  />
                )}
              </FormItem>
              <Button style={{ height: 42 }} type="primary" block loading={loading} onClick={this.login.bind(this)}>登录</Button>
            </Form>
          </div>
          <div className={styles.loginRight}>
            <img src={loginImage} alt="logo" />
          </div>
        </div>
        <div className={styles.footer}>乐山金蜜工业卫士服务股份有限公司 © 2016 蜀 B2-20080224-16</div>
      </div>

    );
  }
}