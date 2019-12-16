import React, { Component } from 'react'
import '@/common/styles/layout/agriculture.less'
import '@/common/styles/theme/agriculture.less'
import { imgGet,openModal } from '@/common/utils'

export default class index extends Component {
  render() {
    return (
      <div className="agriculture_balance_wrap">
        <div className="content_wrap">
          <div className="banner_wrap">
            <div className="banner_info">
              <h3>智慧农业园区系统解决方案</h3>
              <p>精准把控生产问题，支撑合作企业打造智能化、标准化的农业园区</p>
              <div>
                <span>园区智慧照明系统</span>
                <i />
                <span>实时视频监控系统</span>
                <i />
                <span>数据采集系统</span>
                <i />
                <span>设备控制系统</span>
              </div>
            </div>
            <div className="banner_title">
              <div className="into_title">
                <span className="decorate" />
                <span className="title">详情和优势</span>
                <span className="decorate" />
              </div>
              <div className="into_title_bot">DETAILS AND ADVANTAGES</div>
              <div className="into_title_icon">
                <div>
                  <img src={imgGet('icon3', 'supply')} />
                </div>
              </div>
              <p>
                建立园区智慧照明系统，精确控制园区灯光照明，以时间段和光照情况为依据，实现照明系统的自动启闭；建立实时视频监控系统，实时掌握产业园区的各项情况；建立数据采集系统，通过传感设备采集环境数据(墒情监测数据、气象数据等)，智能记录农作物生长环境；建立设备控制系统，统计分析环境数据、远程控制设备运行、云端设置数据阈值，实现农业园区的智能化管理。
              </p>
            </div>
            <div className="banner_box">
              <ul>
                <li>
                  <div>
                    <img src={imgGet(1, 'balance')} alt="" />
                  </div>
                  <div className="banner_text">
                    <p>实时采集农业数据 快速进行统计分析</p>
                  </div>
                </li>
                <li>
                  <div>
                    <img src={imgGet(2, 'balance')} alt="" />
                  </div>
                  <div className="banner_text">
                    <p>建设园区安防系统 保证园区安全</p>
                  </div>
                </li>
                <li>
                  <div>
                    <img src={imgGet(3, 'balance')} alt="" />
                  </div>
                  <div className="banner_text">
                    <p>智能控制农业设备 提高管理效能</p>
                  </div>
                </li>
                <li>
                  <div>
                    <img src={imgGet(4, 'agriculture')} alt="" />
                  </div>
                  <div className="banner_text">
                    <p>信息实时推送 场景协同联动</p>
                  </div>
                </li>
              </ul>
            </div>
          </div>
          {/* <div className="detail_wrap">
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
          </div> */}
          <div className="flow_wrap">
            <div className="into_title">
              <span className="decorate" />
              <span className="title">智慧农业园区系统解决方案示意图</span>
              <span className="decorate" />
            </div>
            <div className="into_title_bot">SCHEMATIC</div>
            <div className="into_title_icon">
              <div>
                <img src={imgGet('icon3', 'supply')} />
              </div>
            </div>
            <div className="flow_img">
              <img src={imgGet('flow', 'agriculture')} />
            </div>
          </div>
          <div className="app_wrap_bg">
            <div className="app_wrap">
              <div className="into_title">
                <span className="decorate" />
                <span className="title">智慧农业园区系统应用案例</span>
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
                    四川某绿地万亩农业产业园的无土栽培研究中心和无土立体循环农业展示区进行建设。其中无土栽培研究中心主要用于研究生产基质肥料，包含多个车间及办公区域，占地面积约40亩，建筑区域约3400平米。无土立体循环农业展示区主要用于展示农业产业园种植手段和科技成果，作为农业产业园的微缩示范区，占地规模约58亩。通过应用物联网技术，实现了对该园区无土栽培中心和无土立体循环农业展示区的实时监控，对园区内的重要农业数据进行了实时采集和统计，提升了园区工作效率，为园区的农业精细化管理提供了重要支撑。
                  </p>
                  <div onClick={openModal}>方案咨询</div>
                </div>
                <div className="info_right">
                  <img src={imgGet('ex', 'agriculture')} alt="" />
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
    )
  }
}
