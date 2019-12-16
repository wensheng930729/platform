package com.bee.platform.dinas.datadriver.service;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.dinas.datadriver.dto.SaleAdjustDetailDTO;
import com.bee.platform.dinas.datadriver.entity.DinasSaleAdjust;
import com.bee.platform.dinas.datadriver.rq.SaleAdjustRQ;

import java.util.List;

/**
 * <p>
 * 销售调价主表 服务类
 * </p>
 *
 * @author huangxin123
 * @since 2019-08-13
 */
public interface DinasSaleAdjustService extends IService<DinasSaleAdjust> {

    /**
     * @descriptin 添加销售合同调价函
     * @author xin.huang
     * @param rq
     * @date 2019/8/14
     * @return
     */
    ResponseResult<ResCodeEnum> add(AuthPlatformUserInfo userInfo, SaleAdjustRQ rq);

    /**
     * @descriptin 编辑销售合同调价函
     * @author xin.huang
     * @param rq
     * @date 2019/8/14
     * @return
     */
    ResponseResult<ResCodeEnum> update(AuthPlatformUserInfo userInfo, SaleAdjustRQ rq);

    /**
     * @descriptin 根据调价函id查询调价函详情列表
     * @author xin.huang
     * @param adjustId
     * @date 2019/8/14
     * @return
     */
    ResponseResult<List<SaleAdjustDetailDTO>> getAdjustDetails(Integer adjustId);

}
