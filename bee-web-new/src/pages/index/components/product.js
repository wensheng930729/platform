import React, { Component } from 'react'
import { Row, Col } from 'antd'
import { utils, go } from 'common'
import '../../../common/styles/theme/home.less'
import { withRouter } from 'react-router'
import Link from 'umi/link'
import router from 'umi/router'
import { connect } from 'dva'

const { imgGet } = utils
const { trade } = go

@withRouter
@connect(({ index }) => ({ index }))
export default class product extends Component {
  constructor(props) {
    super(props)
    this.state = {
      currentPosition: '0rem 0rem',
      currentIndex: 1,
      hover: null,
      key: 'src'
    }
    this.gridList = [
      {
        name: '蜜云智造',
        link: '/brain',
        src: 'gydn',
        key: 1,
        srcAc: 'gydnAc',
        wtkey: 'icon140',
        abstract: '借助工业APP，提供智能数据综合服务，辅助企业科学决策。'
      },
      {
        name: '领蜂供应链',
        link: '/supplyChain',
        src: 'lfgyl',
        key: 5,
        srcAc: 'lfgylAc',
        wtkey: 'icon120',
        abstract: '依托多维度数据，提供产融结合的供应链服务，全面赋能成员企业。'
      },
      {
        name: '蜂客CRM',
        link: '/crm',
        src: 'fkcrm',
        key: 3,
        srcAc: 'fkcrmAc',
        wtkey: 'icon110',
        abstract: '覆盖客户的全面需求，将客户关系管理提升至企业战略层面。'
      },
      {
        name: '蜂巢进销存',
        link: '/erp',
        src: 'fcerp',
        key: 2,
        srcAc: 'fcerpAc',
        wtkey: 'icon120',
        abstract: '实现产供销一体化，通过信息化手段节约供应链成本。'
      },

      {
        name: '蜂掌柜',
        link: '/shopkeeper',
        src: 'fzg',
        key: 4,
        srcAc: 'fzgAc',
        wtkey: 'icon110',
        abstract: '建立经营分析体系，发展精益成本管理。'
      },

      {
        name: '工厂运营',
        link: '/supplyChain',
        src: 'gyqytc',
        key: 6,
        srcAc: 'gyqytcAc',
        wtkey: 'icon110',
        abstract:
          '借助蜜云智造+领蜂供应链，集中力量帮助企业解决运营要素，促进目标企业的可持续经营。'
      },
      {
        name: '集蜂联运',
        link: '/transport',
        src: 'jfly',
        key: 7,
        srcAc: 'jflyAc',
        wtkey: 'icon130',
        abstract:
          '以工业物流数字化服务为基础，整合核心仓储资源，匹配专业运输路径，全面提升物流过程的管理能力。'
      },
      {
        name: '线上蜂贸',
        link: '/ifms',
        src: 'xsfm',
        key: 8,
        wtkey: 'icon110',
        srcAc: 'xsfmAc',
        abstract:
          '基于在线工业贸易解决方案，无缝对接内部管理系统，打造智慧化供应链协同平台。'
      }
    ]
    this.joinTimer = {}
    this.outTimer = {}
  }
  getStyle = item => {
    let nowStyle = {}
    if (item.key === this.state.currentIndex) {
      nowStyle = {
        backgroundImage: `url(${imgGet(item.src, 'index')})`,
        backgroundPosition: this.state.currentPosition
      }
    } else {
      nowStyle = {
        backgroundImage: `url(${imgGet(item.src, 'index')})`,
        backgroundPosition: '0 0'
      }
    }
    return nowStyle
  }

  handleHover = item => {
    console.log(item)
    this.setState({
      hover: item.key,
      key: item.src
    })
    clearInterval(this.outTimer)
    this.gridList.map(inerItem => {
      if (inerItem.key === item.key) {
        let a = inerItem.srcAc
        inerItem.srcAc = inerItem.src
        inerItem.src = a
      }
      return inerItem
    })

    let that = this
    let tempY = 0
    let count = 0
    let cachTemp = '0 0'
    this.setState({
      currentIndex: item.key
    })
    this.joinTimer = setInterval(function() {
      count = count + 1
      if (count > 19) {
        clearInterval(this.joinTimer)
        tempY = 0
        cachTemp = '0rem' + ' ' + '-' + tempY + 'rem'
      } else {
        if (item.name === '集蜂联运') {
          tempY = tempY + 0.938
        } else {
          tempY = tempY + 0.94218
        }
        cachTemp = '0rem' + ' ' + '-' + tempY + 'rem'
      }
      that.setState({
        currentPosition: cachTemp
      })
    }, 50)
    this.getStyle(item)
  }

  handleLeave = item => {
    this.setState({
      hover: null,
      key: item.srcAc
    })
    clearInterval(this.joinTimer)
    this.gridList.map(inerItem => {
      if (inerItem.key === item.key) {
        let a = inerItem.src
        inerItem.src = inerItem.srcAc
        inerItem.srcAc = a
      }
      return inerItem
    })

    let that = this
    let tempY = 0
    let count = 0
    let cachTemp = '0 0'
    this.setState({
      currentIndex: item.key
    })
    this.outTimer = setInterval(function() {
      count = count + 1
      if (count > 19) {
        clearInterval(this.outTimer)
        tempY = 0
        cachTemp = '0' + ' ' + '-' + tempY + 'rem'
      } else {
        if (item.name === '集蜂联运') {
          tempY = tempY + 0.938
        } else {
          tempY = tempY + 0.94218
        }
        cachTemp = '0' + ' ' + '-' + tempY + 'rem'
      }
      that.setState({
        currentPosition: cachTemp
      })
    }, 50)
    this.getStyle(item)
  }

  turnTo = link => {
    this.props.history.push(link)
    window.scrollTo(0, 0)
  }

  render() {
    const { hover, src } = this.state
    return (
      <div className="flex-column productWrap">
        <div className="productMain">
          <div className="moduleTitle">
            <p>
              <span className="codeLine">——</span>产品服务
              <span className="codeLine">——</span>
            </p>
            <p className="partnerName">Products and Services</p>
            <p className="productTitIcon">
              <img
                className="productIconSize"
                src={imgGet('product_title_icon', 'index')}
              />
            </p>
          </div>
          <div className="flex-row productCont">
            {this.gridList.map((item, index) => (
              <div
                className="flex-column productItemWrap"
                key={`productItem${index}`}
              >
                {/* {
                   hover === item.key
                     ?
                     <Link to={item.link} >
                       <img src={imgGet(item.srcAc, 'index')} style={{ cursor: 'pointer' }} />
                     </Link>
                     :
                     <img src={imgGet(item.src, 'index')} />
                 } */}
                {item.name === '线上蜂贸' ? (
                  <div className="flex-column productItem">
                    <a
                      className={[`productItemIcon ${item.wtkey}`]}
                      onClick={() => router.push('/trade')}
                      onMouseEnter={this.handleHover.bind(this, item)}
                      onMouseLeave={this.handleLeave.bind(this, item)}
                      style={this.getStyle(item)}
                    >
                      <cite>{item.name}</cite>
                    </a>
                  </div>
                ) : (
                  <div className="flex-column productItem">
                    <a
                      className={[`productItemIcon ${item.wtkey}`]}
                      onClick={this.turnTo.bind(this, item.link)}
                      onMouseEnter={this.handleHover.bind(this, item)}
                      onMouseLeave={this.handleLeave.bind(this, item)}
                      style={this.getStyle(item)}
                    >
                      <cite>{item.name}</cite>
                    </a>
                  </div>
                )}

                <div className="productItemText">
                  <h3 className="productItemName">{item.name}</h3>
                  <p className="productInfo">{item.abstract}</p>
                </div>
              </div>
            ))}
          </div>
        </div>
      </div>
    )
  }
}
