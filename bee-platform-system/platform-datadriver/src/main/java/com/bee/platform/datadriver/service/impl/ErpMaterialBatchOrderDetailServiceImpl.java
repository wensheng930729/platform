package com.bee.platform.datadriver.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.datadriver.dao.mapper.CommonMapper;
import com.bee.platform.datadriver.dao.mapper.ErpMaterialBatchOrderDetailMapper;
import com.bee.platform.datadriver.dto.ErpMaterialBatchDetailsDTO;
import com.bee.platform.datadriver.dto.ErpMaterialBatchOrderDetailDTO;
import com.bee.platform.datadriver.entity.ErpMaterialBatchOrderDetail;
import com.bee.platform.datadriver.entity.ErpProduct;
import com.bee.platform.datadriver.rq.ErpMaterialBatchOrderDetailRQ;
import com.bee.platform.datadriver.service.ErpMaterialBatchOrderDetailService;
import com.bee.platform.datadriver.service.ErpProductService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;

import java.util.Date;
import java.util.List;

/**
 * <p>
 * 料批明细表 服务实现类
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */

@Slf4j
@Service
public class ErpMaterialBatchOrderDetailServiceImpl extends ServiceImpl<ErpMaterialBatchOrderDetailMapper, ErpMaterialBatchOrderDetail> implements ErpMaterialBatchOrderDetailService {


    @Autowired
    private CommonMapper commonMapper;

    @Autowired
    private ErpProductService productService;

    /**
     * 保存料批明细
     *
     * @param userInfo
     * @param rq
     * @return
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public Integer saveMaterialBatchOrderDetail(AuthPlatformUserInfo userInfo, ErpMaterialBatchOrderDetailRQ rq) {
        Date time = new Date();
        Integer orgId = userInfo.getOrgId();
        Integer userId = userInfo.getId();
        ErpMaterialBatchOrderDetail order = BeanUtils.copyProperties(rq, ErpMaterialBatchOrderDetail.class);
        if (ObjectUtils.isEmpty(rq.getId())) {
            order.setCreateTime(time).setCreatorId(userId).setCreatorEnterpriseId(orgId);
        } else {
            order.setModifierId(userId).setModifyTime(time);
        }
        if (!insertOrUpdate(order)) {
            log.error("保存料批明细失败，调用{}的{}方法出错", "ErpMaterialBatchOrderDetailServiceImpl", "saveMaterialBatchOrderDetail()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_MATERIAL_BATCH_ORDER_DETAIL_SAVE_FAILED);
        }
        return order.getId();
    }

    /**
     * 根据id删除料批明细
     *
     * @param userInfo
     * @param id
     */

    @Transactional(rollbackFor = Exception.class)
    @Override
    public void deleteMaterialBatchOrderDetailById(AuthPlatformUserInfo userInfo, Integer id) {
        Integer userId = userInfo.getId();
        Date time = new Date();
        ErpMaterialBatchOrderDetail exist = selectOne(new EntityWrapper<ErpMaterialBatchOrderDetail>().eq("id", id).eq("deleted", 0));
        if(ObjectUtils.isEmpty(exist)){
            log.info("根据id删除料批详情，没有找到相应数据，id为："+id);
            return;
        }
        if (!updateById(new ErpMaterialBatchOrderDetail().setId(id).setDeleted(1).setModifierId(userId).setModifyTime(time))) {
            log.error("删除料批明细失败，调用{}的{}方法出错", "ErpMaterialBatchOrderDetailServiceImpl", "deleteMaterialBatchOrderDetailById()");
            throw new BusinessException(ResCodeEnum.DELETE_FAILED, ExceptionMessageEnum.ERP_MATERIAL_BATCH_ORDER_DETAIL_DELETE_FAILED);
        }

    }

    /**
     * 根据id查询料批明细产品列表
     *
     * @param id
     * @return
     */
    @Override
    public List<ErpMaterialBatchDetailsDTO> getMaterialBatchDetails(Integer id) {

        return baseMapper.getMaterialBatchDetails(id);
    }

    /**
     * 根据id查询料批明细
     *
     * @param id
     * @return
     */
    @Override
    public ErpMaterialBatchOrderDetailDTO getMaterialBatchDetailById(Integer id) {
        ErpMaterialBatchOrderDetailDTO dto = new ErpMaterialBatchOrderDetailDTO();

        ErpMaterialBatchOrderDetail d = selectOne(new EntityWrapper<ErpMaterialBatchOrderDetail>().eq("id",id).eq("deleted",0));
        if (ObjectUtils.isEmpty(d)) {
            log.info("根据id查询料批明细,没有找到相关数据，id为："+id);
            return dto;
        }
        Integer pId1 = d.getProductId();
        ErpProduct p2 = productService.selectById(pId1);
        if (!ObjectUtils.isEmpty(p2)) {
            d.setProductName(p2.getName());
        }


        dto = BeanUtils.copyProperties(d, ErpMaterialBatchOrderDetailDTO.class);


        return dto;
    }
}
