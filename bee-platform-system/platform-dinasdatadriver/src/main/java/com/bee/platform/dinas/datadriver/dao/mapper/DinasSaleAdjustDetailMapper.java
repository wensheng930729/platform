package com.bee.platform.dinas.datadriver.dao.mapper;

import com.bee.platform.dinas.datadriver.dto.SaleAdjustDetailDTO;
import com.bee.platform.dinas.datadriver.entity.DinasSaleAdjustDetail;
import com.baomidou.mybatisplus.mapper.BaseMapper;

import java.util.List;

/**
 * <p>
 * 销售调价明细表 Mapper 接口
 * </p>
 *
 * @author huangxin123
 * @since 2019-08-13
 */
public interface DinasSaleAdjustDetailMapper extends BaseMapper<DinasSaleAdjustDetail> {

    List<SaleAdjustDetailDTO> findList(Integer adjustId);
}
