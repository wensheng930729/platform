import React, { Component } from 'react';
import styles from './index.less';
import { Breadcrumb, Button, Divider, Modal, Form, Input, Upload, Icon, message } from 'antd';
import Link from 'umi/link';
import router from 'umi/router';
import { domainUrl, path } from '../../../utils/api';
import { get, post, fetchPost } from '../../../utils/fetch';

const FormItem = Form.Item;
const newImgUrl = (url) => {
  if (url && url.indexOf('httpss://') > -1) {
    var newUrlArr = url.split('s://')
    return newUrlArr[0] + '://' + newUrlArr[1]
  } else {
    return url;
  }
}

@Form.create()
export default class Index extends Component {
  constructor(props) {
    super(props);
    this.state = {
      details: {},
      buttonLoading: false,
      modify: false,
      previewVisible: false,
      enclosure: '',
      permit: '',
      certificate: '',
      modalUrl: null
    }
  }

  componentDidMount() {
    const { query } = this.props.location;
    get(path.enterpriseDetails + `?EnterpriseId=${query.id}`, domainUrl)
      .then(res => {
        if ( res.code === 1 && res.object) {
          this.setState({
            details: res.object,
            enclosure: newImgUrl(res.object.enclosure),
            permit: newImgUrl(res.object.permit),
            certificate: newImgUrl(res.object.certificate)
          })
        } else {
          message.error("企业详情查询异常：" + res.msg)
        }
      })
  }

  handleRefuse = () => {
    const { query } = this.props.location;
    const _self = this;
    Modal.confirm({
      title: '拒绝后不能撤回，你确定要拒绝吗？',
      content: '你还要继续吗？',
      onOk() {
        post(path.enterpriseAction + `/${query.id}?type=0`, domainUrl)
          .then(res => {
            if ( res.code === 1) {
              message.success("拒绝成功")
              get(path.enterpriseDetails + `?EnterpriseId=${query.id}`, domainUrl)
                .then(res => {
                  if ( res.code === 1 && res.object) {
                    _self.setState({
                      details: res.object,
                    })
                  } else {
                    message.error("企业详情查询异常：" + res.msg)
                  }
                })
            } else {
              message.error("拒绝失败：" + res.msg)
            }
          })
      },
      onCancel() { },
    });
  }

  handleAgree = () => {
    const { query } = this.props.location;
    const _self = this;
    Modal.confirm({
      title: '通过后不能撤回，你确定要通过吗？',
      content: '你还要继续吗？',
      onOk() {
        post(path.enterpriseAction + `/${query.id}?type=1`, domainUrl)
          .then(res => {
            if ( res.code === 1) {
              message.success("通过成功")
              get(path.enterpriseDetails + `?EnterpriseId=${query.id}`, domainUrl)
                .then(res => {
                  if ( res.code === 1 && res.object) {
                    _self.setState({
                      details: res.object,
                    })
                  } else {
                    message.error("企业详情查询异常：" + res.msg)
                  }
                })
            } else {
              message.error("通过失败：" + res.msg)
            }
          })
      },
      onCancel() { },
    });
  }

  handleCancel = () => {
    let _self = this
    Modal.confirm({
      title: '取消后未保存内容将会丢失',
      content: '你还要继续吗？',
      onOk() {
        _self.setState({
          modify: false
        })
      },
      onCancel() { },
    });
  }

  handleSave = () => {
    const { form: { validateFields }, location: { query } } = this.props;
    const { enclosure, permit, certificate } = this.state;
    let url = '';
    validateFields((err, data) => {
      if (err) return;
      let params = {
        ...data,
        id: query.id
      };
      if (enclosure) {
        params['enclosure'] = enclosure;
      } else {
        return message.error("请上传营业执照图片")
      }
      if (permit) {
        params['permit'] = permit;
      } else {
        return message.error("请上传开户许可证图片")
      }
      if (certificate) {
        params['certificate'] = certificate;
      } else {
        return message.error("请上传企业授权书图片")
      }
      for (let key in params) {
        if (url.indexOf('?') === -1) {
          url += `?${key}=${params[key]}`
        } else {
          url += `&${key}=${params[key]}`
        }
      }

      post(path.enterpriseSave + url, domainUrl)
        .then(res => {
          if ( res.code === 1) {
            message.success("修改企业信息成功")
            this.setState({
              modify: false
            })
            get(path.enterpriseDetails + `?EnterpriseId=${query.id}`, domainUrl)
              .then(res => {
                if ( res.code === 1 && res.object) {
                  this.setState({
                    details: res.object,
                    enclosure: newImgUrl(res.object.enclosure),
                    permit: newImgUrl(res.object.permit),
                    certificate: newImgUrl(res.object.certificate)
                  })
                } else {
                  message.error("企业详情查询异常：" + res.msg)
                }
              })
          } else {
            message.error("企业详情查询异常：" + res.msg)
          }
        })
    })
  }

  upload = (info, type) => {
    if (info.file.status === 'done') {
      message.success('文件上传成功');
      if (type === 'enclosure') {
        this.setState({
          enclosure: info.file.response.data.access_url
        })
      }
      if (type === 'permit') {
        this.setState({
          permit: info.file.response.data.access_url
        })
      }
      if (type === 'certificate') {
        this.setState({
          certificate: info.file.response.data.access_url
        })
      }
    } else if (info.file.status === 'error') {
      message.error(`文件上传失败`);
    }
  }
  render() {
    const { getFieldDecorator } = this.props.form;
    const { details, buttonLoading, modify, previewVisible, enclosure, permit, certificate, modalUrl } = this.state;
    let stateText = '';
    if (details.type === 0) {
      stateText = '未通过'
    } else if (details.type === 1) {
      stateText = '已通过'
    } else if (details.type === 2 || details.type === 3) {
      stateText = '未审核'
    }
    return (
      <div className={styles.container}>
        <div className={styles.header}>
          {
            details && <div className={styles.crumb}>
              <Breadcrumb>
                <Breadcrumb.Item><Link to='/enterprise'>企业管理</Link></Breadcrumb.Item>
                <Breadcrumb.Item>{details.name}</Breadcrumb.Item>
              </Breadcrumb>
              <span className={styles.name}>{details.name}</span>
            </div>
          }
          {
            !modify && details.type === 2 || details.type === 3 && <div className={styles.btnBox}>
              <Button size="large" onClick={this.handleRefuse.bind(this)}>拒绝</Button>
              <Button type="primary" size="large" onClick={this.handleAgree.bind(this)} loading={buttonLoading}>通过</Button>
            </div>
          }
          {
            !modify && details.type == 1 && <div className={styles.btnBox}>
              <Button size="large" onClick={() => router.goBack()}>返回</Button>
              <Button type="primary" size="large" onClick={() => this.setState({ modify: true })} loading={buttonLoading}>修改</Button>
            </div>
          }
          {
            !modify && details.type == 0 && <div className={styles.btnBox}>
              <Button size="large" onClick={() => router.goBack()}>返回</Button>
            </div>
          }
          {
            modify && <div className={styles.btnBox}>
              <Button size="large" onClick={this.handleCancel.bind(this)}>取消</Button>
              <Button type="primary" size="large" onClick={this.handleSave.bind(this)} loading={buttonLoading}>保存</Button>
            </div>
          }
        </div>

        <div className={styles.body}>
          <span className={styles.title}>企业信息</span>
          {
            modify ? <Form className={styles.inputBox}>
              <FormItem className={styles.item} label="企业名称：">
                <div className={styles.temp}>
                  {getFieldDecorator('name', {
                    initialValue: details.name,
                    rules: [{
                      required: true, message: '请输入企业名称',
                    }],
                  })(
                    <Input style={{ width: 300 }} />
                  )}
                </div>
              </FormItem>

              <span className={styles.state}><b>状态：</b>{stateText}</span>

              <FormItem className={styles.item} label="联系电话">
                {getFieldDecorator('contact', {
                  initialValue: details.contact,
                  rules: [{
                    required: true, message: '请输入企业联系电话',
                  }],
                })(
                  <Input style={{ width: 300 }} />
                )}
              </FormItem>
              <FormItem className={styles.item} label="联系人">
                {getFieldDecorator('linkman', {
                  initialValue: details.linkman,
                  rules: [{
                    required: true, message: '请输入企业联系人名字',
                  }],
                })(
                  <Input style={{ width: 300 }} />
                )}
              </FormItem>
              <FormItem className={styles.item} label="企业地址">
                {getFieldDecorator('address', {
                  initialValue: details.address,
                  rules: [{
                    required: true, message: '请输入企业地址',
                  }],
                })(
                  <Input style={{ width: 300 }} />
                )}
              </FormItem>
            </Form> : <div className={styles.infoBox}>
                <span><b>企业名称：</b>{details.name ? details.name : '无'}</span>
                <span><b>状态：</b>{stateText}</span>
                <span><b>联系电话：</b>{details.contact ? details.contact : '无'}</span>
                <span><b>联系人：</b>{details.linkman ? details.linkman : '无'}</span>
                <span><b>企业地址：</b>{details.address ? details.address : '无'}</span>
              </div>
          }
          <Divider />
          <span className={styles.title}>相关证件</span>
          {
            modify ? <div className={styles.uploadBox}>
              <div className={styles.item}>
                <span className={styles.text}>营业执照：</span>
                <Upload
                  name='file'
                  action={path.uploadFile}
                  headers={{
                    'subSysClientid': 'admin-v1.0',
                    'sysToken': sessionStorage.sysToken
                  }}
                  accept='image/jpg,image/jpeg,image/png'
                  showUploadList={false}
                  listType="picture-card"
                  onChange={(info) => this.upload(info, "enclosure")}
                >
                  <Icon type="plus" />
                  <div className="ant-upload-text">上传</div>
                </Upload>
                <div className={styles.img} onClick={() => this.setState({ previewVisible: true, modalUrl: enclosure })}>
                  <img src={enclosure} alt="营业执照" />
                  <span>已上传</span>
                </div>
              </div>
              <div className={styles.item}>
                <span className={styles.text}>开户许可证：</span>
                <Upload
                  name='file'
                  action={path.uploadFile}
                  headers={{
                    'subSysClientid': 'admin-v1.0',
                    'sysToken': sessionStorage.sysToken
                  }}
                  accept='image/jpg,image/jpeg,image/png'
                  showUploadList={false}
                  listType="picture-card"
                  onChange={(info) => this.upload(info, "permit")}
                >
                  <Icon type="plus" />
                  <div className="ant-upload-text">上传</div>
                </Upload>
                <div className={styles.img} onClick={() => this.setState({ previewVisible: true, modalUrl: permit })}>
                  <img src={permit} alt="开户许可证" />
                  <span>已上传</span>
                </div>
              </div>
              <div className={styles.item}>
                <span className={styles.text}>企业授权书：</span>
                <Upload
                  name='file'
                  action={path.uploadFile}
                  headers={{
                    'subSysClientid': 'admin-v1.0',
                    'sysToken': sessionStorage.sysToken
                  }}
                  accept='image/jpg,image/jpeg,image/png'
                  showUploadList={false}
                  listType="picture-card"
                  onChange={(info) => this.upload(info, "certificate")}
                >
                  <Icon type="plus" />
                  <div className="ant-upload-text">上传</div>
                </Upload>
                <div className={styles.img} onClick={() => this.setState({ previewVisible: true, modalUrl: certificate })}>
                  <img src={certificate} alt="企业授权书" />
                  <span>已上传</span>
                </div>
              </div>
            </div> : <div className={styles.imgBox}>
                <div className={styles.item}>
                  <span>营业执照：</span>
                  <img src={enclosure} alt="营业执照" />
                </div>
                <div className={styles.item}>
                  <span>开户许可证：</span>
                  <img src={permit} alt="开户许可证" />
                </div>
                <div className={styles.item}>
                  <span>企业授权书：</span>
                  <img src={certificate} alt="企业授权书" />
                </div>
              </div>
          }
        </div>

        <Modal visible={previewVisible} footer={null} onCancel={() => this.setState({ previewVisible: false })}>
          <img alt="example" style={{ width: '100%' }} src={modalUrl} />
        </Modal>
      </div>
    )
  }
}
