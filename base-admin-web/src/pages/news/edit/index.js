import React, { Component } from 'react';
import styles from './index.less';
import { Icon, Form, Input, Select, Button, Modal, message, Upload } from 'antd';
import 'braft-editor/dist/index.css';
import BraftEditor from 'braft-editor';
import { ContentUtils } from 'braft-utils'
import { domainUrl, newsUrl, path } from '../../../utils/api';
import { get, put, post, fetchPost } from '../../../utils/fetch';
import router from 'umi/router';

const FormItem = Form.Item;
const Option = Select.Option;

@Form.create()
export default class index extends Component {
  constructor(props) {
    super(props);
    this.state = {
      data: {},
      content: BraftEditor.createEditorState(null),
      refresh: false,
      ButtonLoading: false
    }
  }

  componentDidMount() {
    const { query } = this.props.location;
    if (query.id) {
      get(path.newsDetails + `/${query.id}`, newsUrl)
        .then(res => {
          if (res.code === 1 && res.object) {
            this.setState({
              data: res.object.news,
              content: BraftEditor.createEditorState(res.object.news.content),
            })
          } else {
            message.error("获取资讯详情失败")
          }
        })
    }
  }

  handleCancel = () => {
    Modal.confirm({
      title: '取消后未保存内容将会丢失',
      content: '你还要继续吗？',
      onOk() {
        router.goBack();
      },
      onCancel() { },
    });
  }

  handleSave = () => {
    const { location: { query }, form: { validateFields } } = this.props;
    const { content } = this.state;
    validateFields((err, data) => {
      if (err) return;
      // let formBody = new FormData();
      let datas = { ...data }
      datas["content"] = content.toHTML();
      // for (var name in datas) {
      //   formBody.append(name, datas[name]);
      // }
      if (query.id) {
        post(path.newsEdit + `/${query.id}`, domainUrl, datas, 'edit')
          .then(res => {
            if (res.code === 1) {
              message.success("修改资讯成功");
              router.goBack();
            } else {
              message.error("修改资讯失败")
            }
          })
      } else {
        post(path.newsAdd, domainUrl, datas, 'edit')
          .then(res => {
            if (res.code === 1) {
              message.success("发布资讯成功");
              router.goBack();
            } else {
              message.error("发布资讯失败")
            }
          })
      }
    })
  }

  handleChange = (content) => {
    this.setState({
      content,
    })
  }

  uploadHandler = info => {
    if (info.file.status === 'done') {
      this.setState({
        content: ContentUtils.insertMedias(this.state.content, [
          {
            type: 'IMAGE',
            url: info.file.response.object.access_url
          }
        ])
      })
    } else if (info.file.status === 'error') {
      message.error(`图片上传失败`);
    }
  }

  render() {
    const { getFieldDecorator } = this.props.form;
    const { content, data, ButtonLoading } = this.state;
    const controls = [
      'undo', 'redo', 'separator',
      'font-size', 'line-height', 'letter-spacing', 'separator',
      'text-color', 'bold', 'italic', 'underline', 'strike-through', 'separator',
      'superscript', 'subscript', 'remove-styles', 'emoji', 'separator', 'text-indent', 'text-align', 'separator',
      'headings', 'list-ul', 'list-ol', 'blockquote', 'code', 'separator',
      'link', 'separator',
      'hr', 'clear', 'fullscreen'
    ]
    const extendControls = [
      {
        key: 'antd-uploader',
        type: 'component',
        component: (
          <Upload
            name='file'
            action={path.uploadFile}
            headers={{
              'subSysClientid': 'admin-v1.0',
              'sysToken': sessionStorage.sysToken
            }}
            accept='image/jpg,image/jpeg,image/png'
            showUploadList={false}
            onChange={value => this.uploadHandler(value)}
          >
            <button
              type="button"
              className="control-item button upload-button"
              data-title="插入图片"
            >
              <Icon type="picture" theme="filled" />
            </button>
          </Upload>
        )
      }
    ]

    return (
      <div className={styles.container}>
        <div className={styles.empty}></div>
        <div className={styles.content}>
          <Form hideRequiredMark={true}>
            <FormItem label="标题" style={{ display: 'flex' }}>
              {getFieldDecorator('title', {
                initialValue: data.title ? data.title : '',
                rules: [{
                  required: true, message: '请输入标题',
                }],
              })(
                <Input className={styles.input} />
              )}
            </FormItem>
            <FormItem label="发布类型" style={{ display: 'flex' }}>
              {getFieldDecorator('type', {
                initialValue: data.type ? String(data.type) : '0',
                rules: [{
                  required: true, message: '请输入标题',
                }],
              })(
                <Select style={{ width: 400 }}>
                  <Option value="0">头条资讯</Option>
                  <Option value="1">金蜜快讯</Option>
                  <Option value="2">分析评论</Option>
                  <Option value="3">人工智能</Option>
                  <Option value="4">其他资讯</Option>
                </Select>
              )}
            </FormItem>
          </Form>
          <p className={styles.span}>正文</p>
          <div className={styles.editor}>
            <BraftEditor
              value={content}
              controls={controls}
              onChange={this.handleChange}
              extendControls={extendControls}
              contentStyle={{
                height: 500,
                boxShadow: 'inset 0 1px 3px rgba(0,0,0,.1)'
              }} />
          </div>
        </div>

        <div className={styles.bottom}>
          <div>
            {/* <Icon type="check-circle" style={{fontSize: 19, color: 'green'}}/>
            <span>已保存草稿</span>
            <span>17分钟前</span> */}
          </div>
          <div>
            <Button onClick={this.handleCancel.bind(this)}>取消</Button>
            <Button type="primary" loading={ButtonLoading} onClick={this.handleSave.bind(this)}>提交</Button>
          </div>
        </div>
      </div>
    )
  }
}