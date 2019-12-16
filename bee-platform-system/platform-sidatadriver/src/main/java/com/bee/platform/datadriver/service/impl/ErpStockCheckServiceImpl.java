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
import com.bee.platform.datadriver.dao.mapper.ErpStockCheckMapper;
import com.bee.platform.datadriver.dto.ErpStockCheckDTO;
import com.bee.platform.datadriver.dto.ErpStockCheckDetailDTO;
import com.bee.platform.datadriver.dto.ErpStockCheckProductListDTO;
import com.bee.platform.datadriver.dto.ErpStockCheckSearchDTO;
import com.bee.platform.datadriver.entity.*;
import com.bee.platform.datadriver.rq.ErpStockCheckRQ;
import com.bee.platform.datadriver.rq.ErpStockCheckSearchRQ;
import com.bee.platform.datadriver.service.*;
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
 * 库存盘点主单表 服务实现类
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */

@Slf4j
@Service
public class ErpStockCheckServiceImpl extends ServiceImpl<ErpStockCheckMapper, ErpStockCheck> implements ErpStockCheckService {

    @Autowired
    private ErpStockCheckDetailService erpStockCheckDetailService;

    @Autowired
    private ErpStockService erpStockService;

    @Autowired
    private CommonMapper commonMapper;

    @Autowired
    private ErpProductService productService;

    @Autowired
    private ErpProductBatchService productBatchService;

    @Autowired
    private ErpOperationLogService erpOperationLogService;
    @Autowired
    private AuthEnterpriseFeignClient authEnterpriseFeignClient;

    @Override
    public ResponseResult<List<ErpStockCheckSearchDTO>> searchStockCheckByCondition(ErpStockCheckSearchRQ rq, Page page, Integer companyId) {
        Pagination pagination = PageUtils.transFromPage(page);
        rq.setCompanyId(companyId);
        String startTime = rq.getStartTime();
        if (!StringUtils.isEmpty(startTime)) {
            rq.setStartTime(startTime + " 00:00:00");
        }
        String endTime = rq.getEndTime();
        if (!StringUtils.isEmpty(endTime)) {
            rq.setEndTime(endTime + " 23:59:59");
        }

        List<ErpStockCheckSearchDTO> dto =  baseMapper.searchStockCheck(rq,pagination);
        if(CollectionUtils.isEmpty(dto)){
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));
        }
        List<Integer> cIds = dto.stream().map(o->o.getCompanyId()).distinct().collect(Collectors.toList());
        // 设置企业名称
        List<AuthEnterpriseFeignDetailDTO> companyList = authEnterpriseFeignClient.getEnterpriseMoreDetail(cIds).getObject();
        if(!CollectionUtils.isEmpty(companyList)){
            for (ErpStockCheckSearchDTO d : dto) {
                for (AuthEnterpriseFeignDetailDTO e : companyList) {
                    if(d.getCompanyId().equals(e.getId())){
                        d.setCompanyName(e.getName());
                    }
                }
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));

    }

    /**
     * 根据id查询库存盘点
     * @param id
     * @return
     */
    @Override
    public ErpStockCheckDTO getStockCheckById(Integer id) {
        ErpStockCheckDTO dto = new ErpStockCheckDTO();
        // 查询主单
        ErpStockCheck erpStockCheck = selectOne(new EntityWrapper<ErpStockCheck>().eq("id", id).eq("deleted", 0));
        if (ObjectUtils.isEmpty(erpStockCheck)) {
            log.info("根据id查看库存盘点,没有找到相关数据，id为："+id);
            return dto;
        }
        List<AuthEnterpriseFeignDetailDTO> companyList = authEnterpriseFeignClient.getEnterpriseMoreDetail(Lists.newArrayList(erpStockCheck.getCompanyId())).getObject();
        if(!CollectionUtils.isEmpty(companyList)){
            erpStockCheck.setCompanyName(companyList.get(0).getName());
        }

        // 查询详情单
        List<ErpStockCheckDetail> details = erpStockCheckDetailService.selectList(new EntityWrapper<ErpStockCheckDetail>().eq("stock_check_id", id).eq("deleted", 0));
        dto = BeanUtils.copyProperties(erpStockCheck, ErpStockCheckDTO.class);
        List<ErpStockCheckDetailDTO> detailDTOList = BeanUtils.assemble(ErpStockCheckDetailDTO.class, details);
        for (ErpStockCheckDetailDTO d : detailDTOList) {

            ErpProduct p = productService.selectById(d.getProductId());
            ErpProductBatch pb = productBatchService.selectById(d.getProductBatchId());
            if (!ObjectUtils.isEmpty(p)) {
                d.setProductName(p.getName()).setUnit(p.getUnit());
            }
            if (!ObjectUtils.isEmpty(p) && !ObjectUtils.isEmpty(pb)) {
                d.setProductAndBatch(p.getName()+"-"+pb.getBatchName());
            }else if(!ObjectUtils.isEmpty(p) && ObjectUtils.isEmpty(pb)) {
                d.setProductAndBatch(p.getName());
            }
            d.setStorehouse(commonMapper.getRepositoryNameById(d.getRepositoryId()));
            d.setTestCode(commonMapper.getTestCodeById(d.getTestReportId()));
        }

        dto.setDetailDTOList(detailDTOList);

        return dto;
    }

    @Transactional(rollbackFor = Exception.class)
    @Override
    public void deleteStockCheckById(AuthPlatformUserInfo userInfo, Integer id) {
        Date time = new Date();
        Integer userId = userInfo.getId();
        ErpStockCheck exist = selectOne(new EntityWrapper<ErpStockCheck>().eq("id", id).eq("deleted", 0));
        if(ObjectUtils.isEmpty(exist)){
            log.info("根据id删除库存盘点，没有找到相应数据，id为："+id);
            return;
        }

        // 删除主单
        if (!ObjectUtils.isEmpty(exist) && !updateById(new ErpStockCheck().setId(id).setModifierId(userId).setModifyTime(time).setDeleted(1))) {
            log.error("删除库存盘点失败，调用{}的{}方法出错", "ErpStockCheckServiceImpl", "deleteStockCheckById()");
            throw new BusinessException(ResCodeEnum.DELETE_FAILED, ExceptionMessageEnum.ERP_STOCK_CHECK_DELETE_FAILED);
        }

        List<ErpStockCheckDetail> details = erpStockCheckDetailService.selectList(new EntityWrapper<ErpStockCheckDetail>().eq("stock_check_id", id).eq("deleted", 0));
        if (CollectionUtils.isEmpty(details)) {
            log.info("没有详情单，主单号为" + id);
            return;
        }
        List<Integer> detailIds = details.stream().map(ErpStockCheckDetail::getId).collect(Collectors.toList());
        // 依次删除详情
        for (Integer detailId : detailIds) {
            erpStockCheckDetailService.deleteStockCheckDetailById(userInfo, detailId);
        }


    }

    /**
     * 根据id修改库存盘点状态
     * @param userInfo
     * @param id
     * @param state
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public void updateStockCheckState(AuthPlatformUserInfo userInfo, Integer id, Integer state) {
        ErpStockCheck exist = selectOne(new EntityWrapper<ErpStockCheck>().eq("id", id).eq("deleted", 0));
        if(ObjectUtils.isEmpty(exist)){
            log.info("根据id修改库存盘点状态，没有找到相关数据，id为："+id);
            return;
        }

        Integer userId = userInfo.getId();
        Date time = new Date();
        ErpStockCheck order = new ErpStockCheck().setId(id).setState(state).setModifierId(userId).setModifyTime(time);
        if (!updateById(order)) {
            log.error("修改库存盘点状态失败，调用{}的{}方法出错", "ErpStockCheckServiceImpl", "updateStockCheckState()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.ERP_STOCK_CHECK_UPDATE_FAILED);
        }


    }

    @Transactional(rollbackFor = Exception.class)
    @Override
    public Integer saveStockCheck(AuthPlatformUserInfo userInfo, ErpStockCheckRQ rq) {
        Date time = new Date();
        Integer orgId = userInfo.getOrgId();
        Integer userId = userInfo.getId();
        ErpStockCheck order = BeanUtils.copyProperties(rq, ErpStockCheck.class);
        String msg;
        if (ObjectUtils.isEmpty(rq.getId())) {
            msg="新增";
            order.setCreateTime(time).setCreatorId(userId).setCreatorEnterpriseId(orgId).setState(0);
        } else {
            msg="编辑";
            order.setModifierId(userId).setModifyTime(time).setState(0);
        }

        if (!insertOrUpdate(order)) {
            log.error("保存库存盘点失败，调用{}的{}方法出错", "ErpStockCheckServiceImpl", "saveStockCheck()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_STOCK_CHECK_SAVE_FAILED);
        }
        // 保存操作日志
        erpOperationLogService.saveLog(order.getCompanyId(),userInfo,order.getId(),"stock_check",msg);

        return order.getId();
    }

    /**
     * 根据公司id查询盘点产品列表
     *
     * @param companyId
     * @return
     */
    @Override
    public List<ErpStockCheckProductListDTO> getProductListByCompanyId(Integer companyId) {
        List<ErpStockCheckProductListDTO> dto = Lists.newArrayList();
        List<ErpStock> stockList = erpStockService.selectList(new EntityWrapper<ErpStock>().eq("org_id", companyId).eq("deleted", 0));

        for (ErpStock stock : stockList) {
            String productName = commonMapper.getProductNameById(stock.getProductId());
            ErpProductBatch pb = productBatchService.selectById(stock.getProductBatchId());
            ErpStockCheckProductListDTO d = new ErpStockCheckProductListDTO();
            List<AuthEnterpriseFeignDetailDTO> companyList = authEnterpriseFeignClient.getEnterpriseMoreDetail(Lists.newArrayList(stock.getOrgId())).getObject();
            if(!CollectionUtils.isEmpty(companyList)){
                d.setCompanyName(companyList.get(0).getName());
            }
            d.setCompanyId(companyId)
                    .setProductId(stock.getProductId())
                    .setProductName(productName)
                    .setProductBatchId(stock.getProductBatchId())
                    .setRepositoryId(stock.getRepositoryId())
                    .setStorehouse(commonMapper.getRepositoryNameById(stock.getRepositoryId()))
                    .setUnit(stock.getUnit())
                    .setExpectNumber(stock.getInitNum().add(stock.getInStockNum()).subtract(stock.getOutStockNum()));
            if(!StringUtils.isEmpty(productName) && !ObjectUtils.isEmpty(pb)){
                d.setProductAndBatch(productName+"-"+pb.getBatchName());
            }else if(!StringUtils.isEmpty(productName) && ObjectUtils.isEmpty(pb)){
                d.setProductAndBatch(productName);
            }
            dto.add(d);
        }

        return dto;
    }
}
