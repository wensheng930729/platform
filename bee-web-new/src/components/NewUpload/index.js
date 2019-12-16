import { Component } from "react";
import PropTypes from "prop-types";
import { Icon, Modal, Upload, message, Progress } from 'antd';
import s from "./index.less";
import { config } from 'common';

export default class NewUpload extends Component {
  state = {
    previewVisible: false,
    previewImage: null,
  };

  handlePreview = file => {
    this.setState({
      previewImage: file.url || file.thumbUrl,
      previewVisible: true,
    });
  };

  render() {
    const { accept, fileList, number, percent, onChange, beforeUpload } = this.props;
    const list = fileList || [];
    const { previewVisible, previewImage } = this.state;
    const uploadButton = (
      <div className={s.upload}>
        <Icon style={{ fontSize: 20 }} type="plus" />
        <div>上传</div>
      </div>
    );
    return (
      <div className="clearfix">
        <Upload
          accept={accept}
          action={`https://www.beesrv.com/bee-web/api/files/uploadFile`}
          className={s.imgList}
          disabled={percent ? true : false}
          fileList={list}
          listType="picture-card"
          beforeUpload={beforeUpload}
          onChange={onChange}
          onPreview={this.handlePreview}
        >
          {list.length >= number ? null : uploadButton}
          {
            percent && <Progress percent={percent} size="small" />
          }
        </Upload>
        <Modal visible={previewVisible} footer={null} onCancel={() => this.setState({ previewImage: null, previewVisible: false })}>
          <img alt="example" style={{ width: '100%' }} src={previewImage} />
        </Modal>
      </div>
    );
  }
}

//限定控件传入的属性类型
NewUpload.propTypes = {
  accept: PropTypes.string, // 上传文件格式
  fileList: PropTypes.array, // 文件列表
  number: PropTypes.number, // 上传文件数
  onChange: PropTypes.func,  //选中时的回调
  beforeUpload: PropTypes.func, // 上传之前钩子函数
};

//设置默认属性
NewUpload.defaultProps = {
  accept: 'image/jpg, image/jpeg, image/png, .pdf', // 默认接受图片文件、PDF
  fileList: null, //0为图片--1为文件--2为图片或者文件
  number: 1, // 默认最多上传一张图
  onChange: () => false,
  beforeUpload: (file, fileList) => {
    //文件或者图片验证
    // let typeToken = false;
    // if (file.type === "image/jpg") {
    //   typeToken = true;
    // }
    // if (file.type === "image/png") {
    //   typeToken = true;
    // }
    // if (file.type === "image/jpeg") {
    //   typeToken = true;
    // }
    // if (file.type === "image/doc" || file.type === "image/docx") {
    //   typeToken = true;
    // }
    // if (!typeToken) {
    //   message.error("请上传JPG，JPEG，PNG，格式文件");
    // }
    // const isMin2M = file.size / 1024 / 1024 > 2;
    const isMax10M = file.size / 1024 / 1024 < 10; // 最大10M
    // if (!isMin2M) {
    //   message.error("图片最小为2M！");
    // }
    if (!isMax10M) {
      message.error("文件大小最大支持10M！");
    }
    return isMax10M;
  }
};