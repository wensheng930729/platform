package com.bee.platform.datadriver.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.datadriver.dao.mapper.ErpProductBatchMapper;
import com.bee.platform.datadriver.dao.mapper.ErpPurchaseOrderDetailMapper;
import com.bee.platform.datadriver.dao.mapper.ErpSaleOrderDetailMapper;
import com.bee.platform.datadriver.dto.ErpProductBatchListDTO;
import com.bee.platform.datadriver.entity.ErpProductBatch;
import com.bee.platform.datadriver.service.ErpProductBatchService;
import com.google.common.collect.Lists;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.List;

/**
 * <p>
 * 产品批次分类项 服务实现类
 * </p>
 *
 * @author liliang123
 * @since 2019-07-12
 */
@Service
public class ErpProductBatchServiceImpl extends ServiceImpl<ErpProductBatchMapper, ErpProductBatch> implements ErpProductBatchService {

    @Override
    public ResponseResult<List<ErpProductBatchListDTO>> getProductBatchListByProductId(Integer productId) {
        List<ErpProductBatchListDTO> dto = Lists.newArrayList();
        List<ErpProductBatch> list = this.selectList(new EntityWrapper<ErpProductBatch>().eq("product_id", productId).eq("deleted", 0));
        if (CollectionUtils.isEmpty(list)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,dto);
        }
        for (ErpProductBatch batch : list) {
            ErpProductBatchListDTO d = new ErpProductBatchListDTO().setProductBatchId(batch.getId()).setBatchName(batch.getBatchName()).setProductId(batch.getProductId());
            dto.add(d);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,dto);
    }

}
