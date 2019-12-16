import React, { Component } from 'react'
import { Button } from 'antd'
import { utils, go, openModal } from 'common'
import withRouter from 'umi/withRouter'
import '../../common/styles/layout/aboutUs.less'
import '../../common/styles/theme/aboutUs.less'

const { con } = go
const { imgGet } = utils
@withRouter
export default class AboutUs extends Component {
  constructor() {
    super()
    this.state = {
      brainIntro:
        '致力于工业大数据应用，围绕工业生产的供应链、采购、生产、销售、服务等多种环节，提供数据可视化展示、多维数据分析与协同生产等智能数据综合服务，为企业决策提供有力依据，加速推动工业制造的智能化转型升级。',
      productInfo: [
        {
          name: '采购分析',
          iconSrc: 'purchase_icon'
        },
        {
          name: '财务分析',
          iconSrc: 'finance_icon'
        },
        {
          name: '管理分析',
          iconSrc: 'manage_icon'
        },
        {
          name: '人力分析',
          iconSrc: 'person_icon'
        },
        {
          name: '销售分析',
          iconSrc: 'sale_icon'
        }
      ],
      enterpriseValue: [
        {
          name: '效率提升',
          iconSrc: 'computer',
          abstract:
            '聚集工业领域的核心要素，打通全产业链条的数据接口 ，借助系统集成，有效提升工业企业的整体效率。'
        },
        {
          name: '运营增效',
          iconSrc: 'plane',
          abstract:
            '以系统数据为依托，探索企业管理，帮助企业构建健全的生产经营体系，控制成本、降低损失，有效提升企业的运营实绩。'
        },
        {
          name: '营运资本优化',
          iconSrc: 'folder',
          abstract:
            '基于蜜云智造，以数据和风险洞察为依据，帮助企业提升资金端和资产端的协同效率，优化整体供应链营运资金配置。'
        },
        {
          name: '作业安全保障',
          iconSrc: 'shield',
          abstract:
            '实时采集数据，经过数据分析，实现生产设备的故障诊断、预测和优化运行，降低作业事故成本，提升企业安全水平。'
        },
        {
          name: '辅助决策',
          iconSrc: 'head',
          abstract:
            '支持数据挖掘与智能钻取，对采购、销售、财务等关键业务指标进行多维数据分析，全方位支撑领航者科学决策。'
        }
      ]
    }
  }
  render() {
    const { brainIntro, productInfo, enterpriseValue } = this.state
    return (
      <div className="flex-column aboutUsWrap">
        <div className="flex-column bannerWrap">
          <div className="flex-row companyIntro">
            <div className="companyPic">
              <img className="imgSize" src={imgGet('company_pic', 'aboutUs')} />
            </div>
            <div className="companyText">
              <div className="companyTextTit">公司简介</div>
              <div className="companyTextEn">Company Profile</div>
              <div className="conpanyIntrudce">
                <p>
                  明达云服科技集团以四川明达云服科技有限公司为载体，下辖乐山金蜜工业卫士服务股份有限公司、四川明达能源科技有限公司、明达云服资产管理（横琴）有限公司等十余家子公司。公司致力于以金融科技、数字化革新服务中小（制造型）企业。公司主营业务包括工业企业数字化转型服务、供应链金融科技、工业企业投资及运营。
                </p>
                <p>
                  公司自成立以来，本着“创新共赢”的经营理念，先后与四川大学、华为技术有限公司、中国五矿集团、中钢集团、中国远洋海运集团、青山控股集团等多家知名企业建立良好的合作关系。服务黑色金属、有色金属、煤焦、化工化纤等行业的中小企业逾百家，总交易额超16亿元。{' '}
                </p>
              </div>
            </div>
          </div>
        </div>

        <div className="flex-column historyCompanyWrap">
          <div className="moduleTitle">
            <p>
              <span className="codeLine">——</span>发展历程
              <span className="codeLine">——</span>
            </p>
            <p className="partnerName">development history</p>
            <p className="productTitIcon">
              <img
                className="productIconSize"
                src={imgGet('product_title_icon', 'index')}
              />
            </p>
          </div>
          <div className="historyContent">
            <img
              className="productIconSize"
              src={imgGet('history3', 'aboutUs')}
            />
          </div>
        </div>

        <div className="flex-column contactContair">
          <div className="moduleTitle">
            <p>
              <span className="codeLine">——</span>联系我们
              <span className="codeLine">——</span>
            </p>
            <p className="partnerName">development history</p>
            <p className="productTitIcon">
              <img
                className="productIconSize"
                src={imgGet('product_title_icon', 'index')}
              />
            </p>
          </div>
          <div className="flex-row contactWrap">
            <div className="contactText">
              <img className="mapSize" src={imgGet('map_text', 'aboutUs')} />
              <div className="flex-column contactInfo">
                <div className="flex-row contactInfoItem">
                  <div className="textContact">客服热线：028-85350738</div>
                  <div className="contactItemIcon">
                    <img
                      className="iconSize"
                      src={imgGet('headset', 'aboutUs')}
                    />
                  </div>
                </div>
                <div className="flex-row contactInfoItem">
                  <div className="textContact">办公电话：028-85988267</div>
                  <div className="contactItemIcon">
                    <img
                      className="iconSize"
                      src={imgGet('telephone', 'aboutUs')}
                    />
                  </div>
                </div>
                <div className="flex-row contactInfoItem">
                  <div className="textContact">beesrv@foxmail.com</div>
                  <div className="contactItemIcon">
                    <img
                      className="iconSize"
                      src={imgGet('email', 'aboutUs')}
                    />
                  </div>
                </div>
                <div className="flex-row contactInfoItem">
                  <div className="textContact">成都市高新区天泰路112号12层</div>
                  <div className="contactItemIcon">
                    <img
                      className="iconSize"
                      src={imgGet('local', 'aboutUs')}
                    />
                  </div>
                </div>
              </div>
            </div>
            <div className="contactMap">
              <Location></Location>
              {/* <img className="mapSize" src={imgGet("map", "aboutUs")} /> */}
            </div>
          </div>
        </div>
      </div>
    )
  }
}
class Location extends React.Component {
  constructor(props) {
    super(props)
  }

  componentDidMount() {
    this.MP('MCOQMEAuIfqqWNjlgUGO2sMEMMELa0ud').then(BMap => {
      let map = new BMap.Map('bMap')
      var point = new BMap.Point(104.0754, 30.598788)
      map.centerAndZoom(point, 15) // 初始化地图,设置中心点坐标和地图级别
      //   var opts = {
      //     width : 150,     // 信息窗口宽度
      //     height: 20,     // 信息窗口高度
      //     title : ""  // 信息窗口标题
      // }
      // map.centerAndZoom(point, 12);
      var marker = new BMap.Marker(point) // 创建标注
      map.addOverlay(marker) // 将标注添加到地图中

      var label = new BMap.Label('我在四川投资大厦-南楼附近', {
        offset: new BMap.Size(10, -30)
      })
      label.setStyle({
        color: 'black',
        fontSize: '14px',
        height: '24px',
        lineHeight: '24px',
        fontFamily: '微软雅黑',
        border: 'none',
        backgroundColor: '#fff',
        maxWidth: 'initial',
        padding: '0 3px',
        border: 'none',
        boxShadow: '3px 3px 5px lightgrey'
      })
      marker.setLabel(label)
      // var infoWindow = new BMap.InfoWindow("我在这", opts);  // 创建信息窗口对象
      // map.openInfoWindow(infoWindow, map.getCenter());      // 打开信息窗口
    })
  }

  MP(ak) {
    return new Promise(function(resolve, reject) {
      var script = document.createElement('script')
      script.type = 'text/javascript'
      script.dataset.name = 'map'
      script.src = `https://api.map.baidu.com/api?v=3.0&ak=${ak}&callback=init` //callback调用init函数。
      document.head.appendChild(script)
      window.init = () => {
        if (window.BMap) resolve(window.BMap)
      }
    })
  }

  render() {
    return (
      <div className="warkLocationBox">
        <div className="warkLocation">
          <div id="bMap" className="warkbMap" />
        </div>
      </div>
    )
  }
}
