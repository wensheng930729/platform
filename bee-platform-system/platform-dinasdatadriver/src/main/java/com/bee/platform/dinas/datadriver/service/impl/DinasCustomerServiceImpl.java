package com.bee.platform.dinas.datadriver.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.dinas.datadriver.dao.mapper.DinasCustomerMapper;
import com.bee.platform.dinas.datadriver.dao.mapper.DinasProductMapper;
import com.bee.platform.dinas.datadriver.dto.DinasCustomerBoxDTO;
import com.bee.platform.dinas.datadriver.dto.DinasCustomerDetailDTO;
import com.bee.platform.dinas.datadriver.dto.DinasCustomerListDTO;
import com.bee.platform.dinas.datadriver.dto.DinasProductSpecAllocateDTO;
import com.bee.platform.dinas.datadriver.entity.DinasCustomer;
import com.bee.platform.dinas.datadriver.entity.DinasCustomerProduct;
import com.bee.platform.dinas.datadriver.entity.DinasOperationLog;
import com.bee.platform.dinas.datadriver.enums.EnumBusinessType;
import com.bee.platform.dinas.datadriver.rq.DinasCustomerAddRQ;
import com.bee.platform.dinas.datadriver.rq.DinasCustomerQueryRQ;
import com.bee.platform.dinas.datadriver.rq.DinasCustomerUpdateRQ;
import com.bee.platform.dinas.datadriver.rq.DinasProductAndSpecIdRQ;
import com.bee.platform.dinas.datadriver.service.DinasCustomerProductService;
import com.bee.platform.dinas.datadriver.service.DinasCustomerService;
import com.bee.platform.dinas.datadriver.service.DinasOperationLogService;
import com.bee.platform.dinas.datadriver.support.OperateType;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.util.*;
import java.util.stream.Collectors;

/**
 * <p>
 * 客户表 服务实现类
 * </p>
 *
 * @author liliang123
 * @since 2019-08-13
 */
@Service
public class DinasCustomerServiceImpl extends ServiceImpl<DinasCustomerMapper, DinasCustomer> implements DinasCustomerService {

    @Autowired
    private DinasCustomerProductService customerProductService;
    @Autowired
    private DinasCustomerMapper customerMapper;
    @Autowired
    private DinasProductMapper productMapper;
    @Autowired
    private DinasOperationLogService operationLogService;

    /**
     * 添加客户
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult addCustomer(AuthPlatformUserInfo userInfo, DinasCustomerAddRQ rq) {
        Integer orgId = userInfo.getOrgId();
        Integer type = rq.getType();
        String customerName = rq.getCustomerName();
        Date now = new Date();
        // 检测客户名称是否存在
        if (checkNameExists(orgId, type, customerName)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NAME_EXIST);
        }
        // 新数据
        Integer userId = userInfo.getId();
        DinasCustomer customer = new DinasCustomer().setCustomerName(customerName)
                .setStatus(rq.getStatus())
                .setType(type)
                .setCompanyId(orgId)
                .setDeleted(Status.FALSE.getKey())
                .setCreateUser(userId)
                .setCreateTime(now);
        this.insert(customer);
        // 和产品关联关系
        List<DinasProductAndSpecIdRQ> productAndSpecList = rq.getRelateProduct();
        if (!CollectionUtils.isEmpty(productAndSpecList)) {
            this.insertCustomerProductRelate(userInfo, customer, productAndSpecList);
        }
        // 日志
        List<Integer> ids = new ArrayList<>(1);
        ids.add(customer.getId());
        this.insertLog(userInfo, type, ids, OperateType.ADD.getMsg());

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 检测名称是否存在
     */
    private boolean checkNameExists(Integer orgId, Integer type, String customerName) {
        int count = this.selectCount(new EntityWrapper<>(new DinasCustomer()
                .setCompanyId(orgId)
                .setCustomerName(customerName)
                .setDeleted(Status.FALSE.getKey())
                .setType(type)));
        return count > 0;
    }

    /**
     * 添加日志
     */
    private void insertLog(AuthPlatformUserInfo userInfo, Integer type, List<Integer> ids, String operateType) {
        List<DinasOperationLog> logList = Lists.newArrayList();
        Date date = new Date();
        if (type.equals(0)) {
            for (Integer id : ids) {
                logList.add(new DinasOperationLog().setBusinessId(id)
                        .setCompanyId(userInfo.getOrgId())
                        .setOperateMsg(operateType)
                        .setOperator(userInfo.getId())
                        .setOperateTime(date)
                        .setOperatorName(userInfo.getName())
                        .setBusinessType(EnumBusinessType.SUPPLY_CUSTOMER.getValue()));
            }
        } else {
            for (Integer id : ids) {
                logList.add(new DinasOperationLog().setBusinessId(id)
                        .setCompanyId(userInfo.getOrgId())
                        .setOperateMsg(OperateType.ADD.getMsg())
                        .setOperator(userInfo.getId())
                        .setOperateTime(date)
                        .setOperatorName(userInfo.getName())
                        .setBusinessType(EnumBusinessType.ORDER_CUSTOMER.getValue()));
            }
        }
        operationLogService.insertBatch(logList);
    }

    private void insertCustomerProductRelate(AuthPlatformUserInfo userInfo, DinasCustomer customer, List<DinasProductAndSpecIdRQ> productAndSpecList) {
        List<DinasCustomerProduct> relateProductList = Lists.newArrayList();
        for (DinasProductAndSpecIdRQ productAndSpec : productAndSpecList) {
            relateProductList.add(
                    new DinasCustomerProduct()
                            .setProductId(productAndSpec.getProductId())
                            .setProductSpecId(productAndSpec.getProductSpecId())
                            .setCustomerId(customer.getId())
                            .setType(customer.getType())
                            .setCreateTime(new Date())
                            .setCreateUser(userInfo.getId())
                            .setDeleted(Status.FALSE.getKey()));
        }
        customerProductService.insertBatch(relateProductList);
    }


    /**
     * 删除客户
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult deleteCustomer(AuthPlatformUserInfo userInfo, Integer type, List<Integer> ids) {
        // 校验客户是否停用
        int count = this.selectCount(new EntityWrapper<>(new DinasCustomer()
                .setDeleted(Status.FALSE.getKey())
                .setStatus(Status.TRUE.getKey()))
                .in("id", ids));
        if (count > 0) {
            return ResponseResult.buildResponseResult(ResCodeEnum.CUSTOMER_NOT_FORBIDDEN);
        }
        // 删除客户
        DinasCustomer entity = new DinasCustomer().setDeleted(Status.TRUE.getKey())
                .setUpdateTime(new Date())
                .setUpdateUser(userInfo.getId());
        this.update(entity, new EntityWrapper<DinasCustomer>().in("id", ids));
        // 删除客户与产品关联表
        DinasCustomerProduct relateEntity = new DinasCustomerProduct()
                .setDeleted(Status.TRUE.getKey())
                .setUpdateTime(new Date())
                .setUpdateUser(userInfo.getId());
        customerProductService.update(relateEntity, new EntityWrapper<DinasCustomerProduct>().in("customer_id", ids));
        // 日志
        if (type != null) {
            this.insertLog(userInfo, type, ids, OperateType.DELETE.getMsg());
        }

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 编辑
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult updateCustomer(AuthPlatformUserInfo userInfo, DinasCustomerUpdateRQ rq) {
        DinasCustomer customer = this.selectOne(new EntityWrapper<>(new DinasCustomer()
                .setId(rq.getId())
                .setCompanyId(userInfo.getOrgId())
                .setDeleted(Status.FALSE.getKey())));
        if (ObjectUtils.isEmpty(customer)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.CUSTOMER_NOT_EXIST);
        }
        if (!customer.getCustomerName().equals(rq.getCustomerName())) {
            // 检测客户名称是否存在
            if (checkNameExists(customer.getCompanyId(), customer.getType(), rq.getCustomerName())) {
                return ResponseResult.buildResponseResult(ResCodeEnum.NAME_EXIST);
            }
        }
        BeanUtils.copyProperties(rq, customer);
        customer.setUpdateTime(new Date())
                .setUpdateUser(userInfo.getId());
        this.updateById(customer);
        // 更新用户产品关联
        List<DinasProductAndSpecIdRQ> productAndSpecList = rq.getRelateProduct();
        DinasCustomerProduct t = new DinasCustomerProduct()
                .setDeleted(Status.TRUE.getKey())
                .setUpdateTime(new Date())
                .setUpdateUser(userInfo.getId());
        customerProductService.update(t, new EntityWrapper<>(new DinasCustomerProduct().setCustomerId(rq.getId())));
        if (!CollectionUtils.isEmpty(productAndSpecList)) {
            this.insertCustomerProductRelate(userInfo, customer, productAndSpecList);
        }
        // 日志
        List<Integer> ids = new ArrayList<>(1);
        ids.add(customer.getId());
        this.insertLog(userInfo, customer.getType(), ids, OperateType.EDIT.getMsg());

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 切换状态
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult switchCustomer(AuthPlatformUserInfo userInfo, Integer id, Integer status) {
        DinasCustomer customer = this.selectOne(new EntityWrapper<>(new DinasCustomer()
                .setId(id)
                .setCompanyId(userInfo.getOrgId())
                .setDeleted(Status.FALSE.getKey())));
        if (ObjectUtils.isEmpty(customer)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.CUSTOMER_NOT_EXIST);
        }
        if (!status.equals(customer.getStatus())) {
            customer.setStatus(status)
                    .setUpdateUser(userInfo.getId())
                    .setUpdateTime(new Date());
            this.updateById(customer);
        }
        // 日志
        List<Integer> ids = new ArrayList<>(1);
        ids.add(customer.getId());
        this.insertLog(userInfo, customer.getType(), ids, OperateType.EDIT.getMsg());

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 详情
     */
    @Override
    public ResponseResult<DinasCustomerDetailDTO> getCustomerDetail(Integer id) {
        DinasCustomer customer = this.selectOne(new EntityWrapper<>(new DinasCustomer().setId(id).setDeleted(Status.FALSE.getKey())));
        if (ObjectUtils.isEmpty(customer)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.CUSTOMER_NOT_EXIST);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,
                com.bee.platform.common.utils.BeanUtils.copyProperties(customer, DinasCustomerDetailDTO.class));
    }

    /**
     * 客户列表
     */
    @Override
    public ResponseResult<List<DinasCustomerListDTO>> getCustomerList(Page page, Integer orgId, DinasCustomerQueryRQ rq) {
        Pagination pagination = PageUtils.transFromPage(page);
        EntityWrapper<DinasCustomer> wrapper = new EntityWrapper<>(new DinasCustomer()
                .setCompanyId(orgId)
                .setType(rq.getType())
                .setDeleted(Status.FALSE.getKey()));
        wrapper.orderBy("create_time", false);
        if (!StringUtils.isBlank(rq.getCustomerName())) {
            wrapper.like("customer_name", rq.getCustomerName());
        }
        if (!ObjectUtils.isEmpty(rq.getStatus())) {
            wrapper.eq("status", rq.getStatus());
        }
        List<DinasCustomer> customerList = customerMapper.selectPage(pagination, wrapper);
        if (CollectionUtils.isEmpty(customerList)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, new ArrayList<>(0));
        }
        List<DinasCustomerListDTO> dtoList = com.bee.platform.common.utils.BeanUtils.assemble(DinasCustomerListDTO.class, customerList);
        // 查询客户关联的产品
        Map<Integer, DinasCustomerListDTO> dtoMap = dtoList.stream().collect(Collectors.toMap(a -> a.getId(), b -> b));
        List<DinasProductSpecAllocateDTO> relateProductList = productMapper.getRelateProductCustomer(new ArrayList<>(dtoMap.keySet()));
        DinasCustomerListDTO customerDTO;
        String product;
        StringBuilder sb;
        for (DinasProductSpecAllocateDTO specAllocateDTO : relateProductList) {
            customerDTO = dtoMap.get(specAllocateDTO.getCustomerId());
            product = customerDTO.getProduct();
            sb = new StringBuilder(product == null ? "" : product);
            if (StringUtils.isBlank(product)) {
                sb.append(specAllocateDTO.getProductName()).append("-").append(specAllocateDTO.getSpecName());
            } else {
                sb.append(",").append(specAllocateDTO.getProductName()).append("-").append(specAllocateDTO.getSpecName());
            }
            customerDTO.setProduct(sb.toString());
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dtoList, PageUtils.transToPage(pagination));
    }

    /**
     * 下拉框--根据类型查询客户
     */
    @Override
    public ResponseResult<List<DinasCustomerBoxDTO>> getCustomerByType(Integer orgId, Integer type) {
        List<DinasCustomer> customerList = this.selectList(new EntityWrapper<>(new DinasCustomer()
                .setCompanyId(orgId)
                .setDeleted(Status.FALSE.getKey())
                .setStatus(Status.TRUE.getKey())
                .setType(type)));
        if (CollectionUtils.isEmpty(customerList)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, new ArrayList<>(0));
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,
                com.bee.platform.common.utils.BeanUtils.assemble(DinasCustomerBoxDTO.class, customerList));
    }

    /**
     * 下拉框--根据类型和产品查询客户
     */
    @Override
    public ResponseResult<List<DinasCustomerBoxDTO>> getCustomerByTypeAndProduct(Integer orgId, Integer productId, Integer type) {
        HashMap<String, Integer> map = Maps.newHashMap();
        map.put("type", type);
        map.put("orgId", orgId);
        map.put("productId", productId);
        List<DinasCustomerBoxDTO> resultList = customerMapper.getCustomerByTypeAndProduct(map);
        if (CollectionUtils.isEmpty(resultList)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, new ArrayList<>(0));
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, resultList);
    }
}
