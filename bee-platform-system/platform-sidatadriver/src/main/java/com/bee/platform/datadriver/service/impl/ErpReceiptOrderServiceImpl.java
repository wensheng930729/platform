package com.bee.platform.datadriver.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.datadriver.dao.mapper.CommonMapper;
import com.bee.platform.datadriver.dao.mapper.ErpReceiptOrderDetailMapper;
import com.bee.platform.datadriver.dao.mapper.ErpReceiptOrderMapper;
import com.bee.platform.datadriver.dto.ErpReceiptOrderDTO;
import com.bee.platform.datadriver.dto.ErpReceiptOrderDetailDTO;
import com.bee.platform.datadriver.dto.ErpReceiptOrderSearchDTO;
import com.bee.platform.datadriver.entity.ErpReceiptOrder;
import com.bee.platform.datadriver.entity.ErpReceiptOrderDetail;
import com.bee.platform.datadriver.entity.ErpSaleOrder;
import com.bee.platform.datadriver.rq.ErpReceiptOrderDetailRQ;
import com.bee.platform.datadriver.rq.ErpReceiptOrderRQ;
import com.bee.platform.datadriver.rq.ErpReceiptSearchRQ;
import com.bee.platform.datadriver.service.ErpOperationLogService;
import com.bee.platform.datadriver.service.ErpReceiptOrderDetailService;
import com.bee.platform.datadriver.service.ErpReceiptOrderService;
import com.bee.platform.datadriver.service.ErpSaleOrderService;
import com.bee.platform.user.authority.dto.AuthEnterpriseFeignDetailDTO;
import com.bee.platform.user.service.feign.AuthEnterpriseFeignClient;
import com.google.common.collect.Lists;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

/**
 * <p>
 * 销售收款单主表 服务实现类
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */
@Slf4j
@Service
public class ErpReceiptOrderServiceImpl extends ServiceImpl<ErpReceiptOrderMapper, ErpReceiptOrder> implements ErpReceiptOrderService {

    @Autowired
    private ErpReceiptOrderDetailService erpReceiptOrderDetailService;
    @Autowired
    private ErpReceiptOrderDetailMapper erpReceiptOrderDetailMapper;
    @Autowired
    private ErpSaleOrderService erpSaleOrderService;
    @Autowired
    private CommonMapper commonMapper;
    @Autowired
    private ErpOperationLogService erpOperationLogService;
    @Autowired
    private AuthEnterpriseFeignClient authEnterpriseFeignClient;
    /**
     * 保存销售收款单
     *
     * @param rq 请求参数
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public Integer saveReceiptOrder(AuthPlatformUserInfo userInfo, ErpReceiptOrderRQ rq) {
        Date time = new Date();
//        BigDecimal zero4Big = new BigDecimal(0);
        String code = rq.getCode();
//        BigDecimal amount = rq.getAmount();
        Integer companyId = rq.getCompanyId();
        Integer orgId = userInfo.getOrgId();
        Integer userId = userInfo.getId();
        List<ErpReceiptOrderDetailRQ> detail = rq.getDetailRQList();
        if(CollectionUtils.isEmpty(detail)){
            log.info("没有详情单信息，不能创建订单");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.ERP_RECEIPT_ORDER_SAVE_FAILED_NO_DETAIL);
        }
        for (ErpReceiptOrderDetailRQ dr : detail) {
            if(ObjectUtils.isEmpty(dr.getReceiptAmount())){
                log.info("详情单没有本次收款金额信息，不能创建订单");
                throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.ERP_RECEIPT_ORDER_SAVE_FAILED_NO_DETAIL_MONEY);

            }
        }

//        BigDecimal detailTotalMoney = detail.stream().map(ErpReceiptOrderDetailRQ::getReceiptAmount).reduce(BigDecimal::add).orElse(zero4Big);
//        if (amount.compareTo(detailTotalMoney) != 0) {
//            log.error("收款金额和详情单本次收款金额不一置，收款金额为{}，详情本次收款总额为" + amount + detailTotalMoney);
//            log.error("收款金额和详情单本次收款金额不一置，调用{}的{}方法出错", "ErpReceiptOrderServiceImpl", "saveReceiptOrder()");
//            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.ERP_RECEIPT_ORDER_SAVE_FAILED_MONEY_D);
//        }
        List<ErpReceiptOrder> exist = selectList(new EntityWrapper<ErpReceiptOrder>().eq("code", code).eq("company_id", companyId).eq("deleted", 0));
        // 新增校验编号重复
        if (ObjectUtils.isEmpty(rq.getId()) && !CollectionUtils.isEmpty(exist)) {
            log.info("收款单号编码重复，编码为:" + code);
            log.info("保存销售收款单失败，收款单号编码重复，调用{}的{}方法出错", "ErpReceiptOrderServiceImpl", "saveReceiptOrder()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.ERP_RECEIPT_ORDER_SAVE_FAILED_CODE_RE);
        }
        // 修改校验编号重复
        if(!ObjectUtils.isEmpty(rq.getId()) && !CollectionUtils.isEmpty(exist) && !exist.get(0).getId().equals(rq.getId())){
            log.info("收款单号编码重复，编码为:" + code);
            log.info("保存销售收款单失败，收款单号编码重复，调用{}的{}方法出错", "ErpReceiptOrderServiceImpl", "saveReceiptOrder()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.ERP_RECEIPT_ORDER_SAVE_FAILED_CODE_RE);
        }


        //保存主表信息
        ErpReceiptOrder erpReceiptOrder = BeanUtils.copyProperties(rq, ErpReceiptOrder.class).setCreatorId(userId).setCreatorEnterpriseId(orgId).setCreateTime(time).setState(0);
        if (!insertOrUpdate(erpReceiptOrder)) {
            log.error("保存销售收款单信息失败，调用{}的{}方法出错", "ErpReceiptOrderServiceImpl", "saveReceiptOrder()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.ERP_RECEIPT_ORDER_SAVE_FAILED);
        }

        Integer pid = erpReceiptOrder.getId();
        // 查询有没有历史详情
        List<ErpReceiptOrderDetail> detailList = erpReceiptOrderDetailService.selectList(new EntityWrapper<ErpReceiptOrderDetail>().eq("receipt_order_id", pid).eq("deleted", 0));
        if (!CollectionUtils.isEmpty(detailList)) {
            // 删除旧的详情
            List<Integer> dIds = detailList.stream().map(ErpReceiptOrderDetail::getId).collect(Collectors.toList());
            for (Integer did : dIds) {
                // 依次删除旧的详情
                erpReceiptOrderDetailService.deleteDetail(userInfo, did);
            }

        }


        // 设置主表id到子表
        List<ErpReceiptOrderDetailRQ> detailRQList = detail.stream().map(o -> o.setReceiptOrderId(erpReceiptOrder.getId())).collect(Collectors.toList());

        // 依次保存详情信息
        for (ErpReceiptOrderDetailRQ detailRQ : detailRQList) {

            erpReceiptOrderDetailService.saveReceiptOrderDetail(userInfo, detailRQ);

        }
        String msg;
        if (ObjectUtils.isEmpty(rq.getId())) {
            msg="新增";
        } else {
            msg="编辑";
        }
        // 保存操作日志
        erpOperationLogService.saveLog(erpReceiptOrder.getCompanyId(),userInfo,erpReceiptOrder.getId(),"sale_receipt",msg);

        return erpReceiptOrder.getId();
    }




    /**
     * 查看收款单信息
     *
     * @param id 收款单id
     * @return
     */
    @Override
    public ErpReceiptOrderDTO getReceiptOrderById(Integer id) {

        ErpReceiptOrderDTO dto = new ErpReceiptOrderDTO();
        // 查询收款单主表
        ErpReceiptOrder erpReceiptOrder = selectOne(new EntityWrapper<ErpReceiptOrder>().eq("id",id).eq("deleted",0));
        if (ObjectUtils.isEmpty(erpReceiptOrder)) {
            log.info("根据id查看销售收款,没有找到相关数据，id为："+id);
            return dto;
        }
        dto = BeanUtils.copyProperties(erpReceiptOrder, ErpReceiptOrderDTO.class);
        String companyName =null;
        List<AuthEnterpriseFeignDetailDTO> companyNames = authEnterpriseFeignClient.getEnterpriseMoreDetail(Lists.newArrayList(erpReceiptOrder.getCompanyId())).getObject();
        if(!CollectionUtils.isEmpty(companyNames)){
            companyName = companyNames.get(0).getName();
        }
        // 获取公司名称 客户名称
        String customerName = commonMapper.getCustomerNameById(erpReceiptOrder.getCustomerId());
        ErpSaleOrder erpSaleOrder = erpSaleOrderService.selectById(erpReceiptOrder.getSaleOrderId());
        if(!ObjectUtils.isEmpty(erpSaleOrder)){
            dto.setSaleCode(erpSaleOrder.getContractNo());
        }
        dto.setCompanyName(companyName).setCustomerName(customerName);

        // 查询详情表信息
        List<ErpReceiptOrderDetailDTO> detailDTOS = erpReceiptOrderDetailMapper.getDetailList(id);

        dto.setDetailDTOList(detailDTOS);

        return dto;
    }

    /**
     * 修改收款单确认状态
     *
     * @param userInfo
     * @param id
     * @param state
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public void updateReceiptOrderState(AuthPlatformUserInfo userInfo, Integer id, Integer state) {
        ErpReceiptOrder exist = selectOne(new EntityWrapper<ErpReceiptOrder>().eq("id", id).eq("deleted", 0));
        if(ObjectUtils.isEmpty(exist)){
            log.info("根据id修改销售收款状态，没有找到相关数据，id为："+id);
            return;
        }

        Integer userId = userInfo.getId();
        Date time = new Date();
        ErpReceiptOrder order = new ErpReceiptOrder().setId(id).setState(state).setModifierId(userId).setModifyTime(time);
        if (!updateById(order)) {
            log.error("修改销售收款单确认状态失败，调用{}的{}方法出错", "ErpReceiptOrderServiceImpl", "updateReceiptOrderState()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.ERP_RECEIPT_ORDER_UPDATE_FAILED);
        }

    }

    /**
     * 条件查询销售收款列表
     *
     * @param
     * @param rq
     * @param page
     * @return
     */
    @Override
    public ResponseResult<List<ErpReceiptOrderSearchDTO>> searchReceiptByCondition(ErpReceiptSearchRQ rq, Page page, Integer companyId) {
        Pagination pagination = PageUtils.transFromPage(page);
//        List<ErpReceiptOrderSearchDTO> dto = Lists.newArrayList();
//        List<AuthEnterpriseFlatDTO> enterpriseFlatDTOS = authEnterpriseFeignClient.getEnterpriseFlatByCompanyId(companyId).getObject();
//        if (org.springframework.util.CollectionUtils.isEmpty(enterpriseFlatDTOS)) {
//            log.error("未查询到当前用户企业信息！ 类：{} 方法：{}", "ErpReceiptOrderServiceImpl", "searchReceiptByCondition");
//            return ResponseResult.buildResponseResult(ResCodeEnum.GET_ENTERPRISE_USER_RELATION_FAILED, Lists.newArrayList(), PageUtils.transToPage(pagination));
//        }
//        List<Integer> ids = enterpriseFlatDTOS.stream().map(a -> a.getValue()).collect(Collectors.toList());
//        if (CollectionUtils.isEmpty(ids)) {
//            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));
//        }
//        rq.setList(ids);
        rq.setCompanyId(companyId);
        String startTime = rq.getStartTime();
        if (!StringUtils.isEmpty(startTime)) {
            rq.setStartTime(startTime + " 00:00:00");
        }
        String endTime = rq.getEndTime();
        if (!StringUtils.isEmpty(endTime)) {
            rq.setEndTime(endTime + " 23:59:59");
        }

        List<ErpReceiptOrderSearchDTO> dto = baseMapper.searchReceiptOrderByCondition(rq,pagination);
        if(CollectionUtils.isEmpty(dto)){
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));
        }
        List<Integer> cIds = dto.stream().map(o->o.getCompanyId()).distinct().collect(Collectors.toList());
        // 设置企业名称
        List<AuthEnterpriseFeignDetailDTO> companyList = authEnterpriseFeignClient.getEnterpriseMoreDetail(cIds).getObject();
        if(!CollectionUtils.isEmpty(companyList)){
            for (ErpReceiptOrderSearchDTO d : dto) {
                for (AuthEnterpriseFeignDetailDTO e : companyList) {
                    if(d.getCompanyId().equals(e.getId())){
                        d.setCompanyName(e.getName());
                    }
                }
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));

    }


    @Transactional(rollbackFor = Exception.class)
    @Override
    public void deleteReceiptOrderById(AuthPlatformUserInfo userInfo, Integer id) {

        Integer userId = userInfo.getId();
        Date time = new Date();
        ErpReceiptOrder exist = selectOne(new EntityWrapper<ErpReceiptOrder>().eq("id", id).eq("deleted", 0));
        if(ObjectUtils.isEmpty(exist)){
            log.info("根据id删除销售收款，没有找到相应数据，id为："+id);
            return;
        }
        // 删除主单信息
        if (!updateById(new ErpReceiptOrder().setId(id).setDeleted(1).setModifierId(userId).setModifyTime(time))) {
            log.error("删除销售收款失败，调用{}的{}方法出错", "ErpReceiptOrderServiceImpl", "deleteReceiptOrderById()");
            throw new BusinessException(ResCodeEnum.DELETE_FAILED, ExceptionMessageEnum.ERP_RECEIPT_ORDER_DELETE_FAILED);
        }

        List<ErpReceiptOrderDetail> detailList = erpReceiptOrderDetailService.selectList(new EntityWrapper<ErpReceiptOrderDetail>().eq("receipt_order_id", id).eq("deleted", 0));
        if(CollectionUtils.isEmpty(detailList)){
            log.info("根据id删除销售收款详情，没有找到相应数据，主单id为："+id);
            return;
        }

        List<Integer> ids = detailList.stream().map(ErpReceiptOrderDetail::getId).collect(Collectors.toList());
        for (Integer detailId : ids) {
            // 删除详情单
            erpReceiptOrderDetailService.deleteDetail(userInfo, detailId);
        }


    }
}
