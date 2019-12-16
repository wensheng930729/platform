package com.bee.platform.datadriver.service;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.datadriver.dto.ErpProductBatchListDTO;
import com.bee.platform.datadriver.entity.ErpProductBatch;

import java.util.List;

/**
 * <p>
 * 产品批次分类项 服务类
 * </p>
 *
 * @author liliang123
 * @since 2019-07-12
 */
public interface ErpProductBatchService extends IService<ErpProductBatch> {

    /**
     * 根据产品id查询产品批次列表
     *
     * @param productId 产品id
     * @return 产品批次列表
     * @Author ck
     */
    public ResponseResult<List<ErpProductBatchListDTO>> getProductBatchListByProductId(Integer productId);
}
