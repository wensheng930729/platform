import React, { Component } from 'react'
import '@/common/styles/layout/balance.less'
import '@/common/styles/theme/balance.less'
import { imgGet,openModal } from '@/common/utils'

export default class index extends Component {
  render() {
    return (
      <div className="balance_wrap">
        <div className="content_wrap">
          <div className="banner_wrap">
            <div className="banner_info">
              <h3>智能过磅系统解决方案</h3>
              <p>
                实现故障的智能诊断和可能性预测，为预测预警、溯源治理等环保决策提供数据支持，高效解决工厂在环境监测中的诸多难点
              </p>
            </div>
            <div className="banner_box">
              <ul>
                <li>
                  <div>
                    <img src={imgGet(1, 'balance')} alt="" />
                  </div>
                  <div className="banner_text">
                    <p>解决“信息孤岛”问题 </p>
                    <p>全面提升企业信息化管理水平</p>
                  </div>
                </li>
                <li>
                  <div>
                    <img src={imgGet(2, 'balance')} alt="" />
                  </div>
                  <div className="banner_text">
                    <p>运用数字化信息管理手段</p>
                    <p>解决地磅数据无法实时查看的问题</p>
                  </div>
                </li>
                <li>
                  <div>
                    <img src={imgGet(3, 'balance')} alt="" />
                  </div>
                  <div className="banner_text">
                    <p>解决工厂地磅计量可能存在的</p>
                    <p>基础化称重作弊问题</p>
                    <p>以及地磅常规性管理问题</p>
                  </div>
                </li>
              </ul>
            </div>
          </div>
          <div className="detail_wrap">
            <div className="detail_flag" />
            <div className="detail_title">
              <h2>详情和优势</h2>
              <p className="into_title_bot">DETAILS AND ADVANTAGES</p>
              <div className="into_title_icon">
                <div>
                  <img src={imgGet('icon3', 'supply')} />
                </div>
              </div>
            </div>
            <div className="position_wrap">
              <div className="img_top">
                <img src={imgGet('right', 'balance')} alt="" />
              </div>
              <div className="img_left">
                <img src={imgGet('left', 'balance')} alt="" />
              </div>
              <div className="text_bottom">
                利用企业现有地磅，结合视频监控和车牌识别系统等，对进出的货物材料，进行自动登记。车辆进出场称重时，自动识别出车牌，上磅时自动读取过磅的重量信息，并抓拍图片。同时，系统通过对比自动算出该车的相关数据信息，生成报表。报表信息包括车辆的进出场信息、车牌图片、车辆对比图片等，在有效解决上述问题的基础上，加强企业在供应链方面的精细化管理。
              </div>
            </div>
          </div>
          <div className="flow_wrap">
            <div className="into_title">
              <span className="decorate" />
              <span className="title">智能过磅系统解决方案示意图</span>
              <span className="decorate" />
            </div>
            <div className="into_title_bot">SCHEMATIC</div>
            <div className="into_title_icon">
              <div>
                <img src={imgGet('icon3', 'supply')} />
              </div>
            </div>
            <div className="flow_img">
              <img src={imgGet('flow', 'balance')} />
            </div>
          </div>
          <div className="app_wrap_bg">
            <div className="app_wrap">
              <div className="into_title">
                <span className="decorate" />
                <span className="title">智能过磅系统应用案例</span>
                <span className="decorate" />
              </div>
              <div className="into_title_bot">APPLICATION SCENARIOS</div>
              <div className="into_title_icon">
                <div>
                  <img src={imgGet('icon3', 'supply')} />
                </div>
              </div>
              <div className="app_info">
                <div className="info_left">
                  <h5>项目概述</h5>
                  <p>
                    四川某铁合金公司从事铬铁合金制造二十余年。该企业原料运输收货量每年高达数十万吨，高碳铬铁运输发货量高达十余万吨，每日进出货运量较大。为有效提升车辆过磅效率，该公司采用蜂创物联的智能过磅解决方案，通过结合微波射频识别技术、通讯技术、数据库技术以及工业网络，将称重结果、车辆信息和时间信息等自动写入主机数据库，在充分提高企业作业生产水平的基础上，还为企业的持续发展注入了新的力量。
                  </p>
                  <div onClick={openModal}>方案咨询</div>
                </div>
                <div className="info_right">
                  <img src={imgGet('bottom', 'balance')} alt="" />
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
    )
  }
}
