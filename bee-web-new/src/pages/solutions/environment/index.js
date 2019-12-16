import React, { Component } from 'react'
import 'common/styles/layout/environment.less'
import { utils } from 'common'
const { imgGet,openModal } = utils

export default class index extends Component {
    render() {
        return (
            <div className='environmentWrap'>
                <div className='environmentContent'>
                    <div className='environmentBannerContent'>
                        <div className='environmentTitle'>
                        环境监测系统解决方案
                        </div>
                        <div className='environmentDesc'>
                            <p>
                            实现故障的智能诊断和可能性预测，为预测预警、溯源治理等环保决策提供数据支持，高效解决工厂在环境监测中的诸多难点。
                            </p>
                        </div>
                    </div>
                    <div className='environmentBannerDetail'>
                        <div className='environmentBannerDetailTitle'>
                            <h3><span className='environmentBannerDetailTitleLeft'>——</span>详情和优势<span className='environmentBannerDetailTitleRight'>——</span></h3>
                            <h4>Details and advantages</h4>
                            <h5><img className="productIconSize" src={imgGet("product_title_icon", "index")} /></h5>
                        </div>
                        <div className='environmentBannerDetailContent'>
                            <p>
                            依托采集终端、数据监测、通信传输等集成应用，实时监测企业在生产过程中污染物排放数据，并通过设置环境超标自动报警系统，实现故障的智能诊断和可能性预测，高效解决工厂在环境监测中的诸多难点。
                            </p>
                            <div className='environmentBannerDetailContentImg'>
                                <div className='environmentBannerDetailContentImgLeft environmentBannerDetailContentImgBox'>
                                    <p>实时监测污染物排放情况</p>
                                </div>
                                <div className='environmentBannerDetailContentImgMiddle environmentBannerDetailContentImgBox'>
                                <p>污染物排放超标报警</p>
                                </div>
                                <div className='environmentBannerDetailContentImgRight environmentBannerDetailContentImgBox'>
                                <p>降低突发事故发生率</p>
                                </div>
                            </div>
                        </div>
                    </div>
                    <div className='environmentSolutions'>
                    <h3><span className='environmentSolutionsTitleLeft'>——</span>环境监测系统解决方案<span className='environmentSolutionsTitleRight'>——</span></h3>
                            <h4>schematic</h4>
                            <h5><img className="productIconSize" src={imgGet("product_title_icon", "index")} /></h5>
                            <div className='environmentSolutionsImg'>

                            </div>
                    </div>
                </div>
                
                <div className='environmentExample'>
                    <div className='environmentExampleContent'>
                    <h3><span className='environmentExampleContentTitleLeft'>——</span>环境监测系统应用案例<span className='environmentExampleContentTitleRight'>——</span></h3>
                            <h4>Application scenarios</h4>
                            <h5><img className="productIconSize" src={imgGet("product_title_icon", "index")} /></h5>
                            <div className='environmentExampleContentDetail'>
                                <div className='environmentExampleContentDetailLeft'>
                                    <h3>项目概述</h3>
                                    <p>
                                    四川雅安某镍业发展有限公司从事镍铬合金冶炼十余年，是典型的“三高企业”。在“金山银山，不如绿水青山”的政策引导下，针对高污染问题，该公司借助蜂创物联的环境监测管理系统，依托采集终端、数据监测、通信传输等集成应用，实时监测企业在生产过程中污染物排放数据，并通过设置环境超标自动报警系统，实现故障的智能诊断和可能性预测，高效解决工厂在环境监测中的诸多难点。
                                    </p>
                                    <div className='environmentAsk' onClick={openModal}>方案咨询</div>
                                </div>
                                <div className='environmentExampleContentDetailRight'>

                                </div>
                            </div>
                    </div>
                </div>
            </div>
        )
    }
}
