package com.bee.platform.customer.service.impl;

import com.alibaba.fastjson.JSON;
import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.constants.enums.EnumCommon;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.service.JedisService;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.customer.dao.mapper.AuthCustomerMapper;
import com.bee.platform.customer.dto.*;
import com.bee.platform.customer.entity.*;
import com.bee.platform.customer.rq.AuthContactUpdateRQ;
import com.bee.platform.customer.rq.AuthCustomerAddRQ;
import com.bee.platform.customer.rq.AuthCustomerSelectRQ;
import com.bee.platform.customer.rq.AuthCustomerUpdateRQ;
import com.bee.platform.customer.service.*;
import com.bee.platform.datadriver.entity.ErpCode;
import com.bee.platform.datadriver.service.CommonService;
import com.bee.platform.datadriver.service.ErpCodeService;
import com.bee.platform.user.authority.dto.AuthEnterpriseFlatDTO;
import com.bee.platform.user.service.feign.AuthEnterpriseFeignClient;
import com.google.common.collect.Lists;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.util.*;
import java.util.stream.Collectors;

/**
 * <p>
 * 服务实现类
 * </p>
 *
 * @author hongchuan.He
 * @since 2019-05-20
 */
@Slf4j
@Service
public class AuthCustomerServiceImpl extends ServiceImpl<AuthCustomerMapper, AuthCustomer> implements AuthCustomerService {

    @Autowired
    private AuthCustomerMapper customerMapper;
    @Autowired
    private AuthContactService contactService;
    @Autowired
    private AuthCustomerContactService customerContactService;
    @Autowired
    private ErpCustomerCategoryRelationService relationService;
    @Autowired
    private ErpCustomerCategoryService customerCategoryService;
    @Autowired
    private ErpCodeService codeService;
    @Autowired
    private JedisService jedisService;
    @Autowired
    private AuthEnterpriseFeignClient enterpriseFeignClient;
    @Autowired
    private CommonService commonService;


    /**
     * 添加客户
     *
     * @param rq
     * @return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<Integer> addCustomer(AuthCustomerAddRQ rq, AuthPlatformUserInfo userInfo) {
        // 企业id
        Integer orgId = null;
        if (!ObjectUtils.isEmpty(rq.getEnterpriseId())) {
            orgId = rq.getEnterpriseId();
        } else {
            orgId = userInfo.getOrgId();
        }

        AuthCustomer authCustomer = BeanUtils.copyProperties(rq, AuthCustomer.class);
        authCustomer.setCusFirstType(JSON.toJSONString(rq.getCusFirstType()))
                .setCusSecondType(JSON.toJSONString(rq.getCusSecondType()))
                .setEnterpriseId(orgId)
                .setCreateTime(new Date())
                .setOperateId(userInfo.getId())
                .setStatus(EnumCommon.IsActive.is_active.getKey())
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey());
        // 客户编码
        String redisKey = "customer" + orgId;
        int customerCode = commonService.getRedisCode(redisKey);
        String cusNo = commonService.getCode(orgId.toString(), 4) + commonService.getCode(customerCode + "", 4);
        authCustomer.setCusNo(cusNo);
        // 将自增的序号set回redis
        jedisService.set(redisKey, (customerCode + 1) + "", 0);

        if (!this.insert(authCustomer)) {
            log.error("添加客户失败 类{} 方法{}", "AuthCustomerServiceImpl", "add");
            return ResponseResult.buildResponseResult(ResCodeEnum.ADD_FAILED);
        }
        // 记录客户，二级分类 关联关系
        List<ErpCustomerCategoryRelation> relationList = Lists.newArrayList();
        for (Integer integer : rq.getCusSecondType()) {
            relationList.add(new ErpCustomerCategoryRelation()
                    .setCustomerId(authCustomer.getId())
                    .setCategoryId(integer)
                    .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey()));
        }
        relationService.insertBatch(relationList);

        /**------------------------------联系人相关------------------------------------*/
        // 添加客户联系人
        if (!CollectionUtils.isEmpty(rq.getContactList())) {
            List<AuthContact> contactList = BeanUtils.assemble(AuthContact.class, rq.getContactList());
            contactList.forEach(a -> a
                    .setCreateTime(new Date())
                    .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                    .setStatus(EnumCommon.IsActive.is_active.getKey()));
            if (!contactService.insertBatch(contactList)) {
                log.error("记录联系人失败 类：{}方法：{}", "AuthCustomerServiceImpl", "addCustomer");
                throw new BusinessException(ResCodeEnum.ADD_FAILED, ExceptionMessageEnum.CONTACT_ADD_FAILED);
            }
            // 添加客户和联系人之间的关系
            List<AuthCustomerContact> customerContactList = Lists.newArrayList();
            for (AuthContact contact : contactList) {
                customerContactList.add(new AuthCustomerContact()
                        .setContactId(contact.getId())
                        .setCustomerId(authCustomer.getId())
                        .setCreateTime(new Date())
                        .setCreateUser(userInfo.getName())
                        .setOperateId(userInfo.getId())
                        .setStatus(EnumCommon.IsActive.is_active.getKey())
                        .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey()));
            }
            if (!CollectionUtils.isEmpty(customerContactList) && customerContactList.size() > 0) {
                if (!customerContactService.insertBatch(customerContactList)) {
                    log.error("记录客户与联系人关联关系失败 类：{}方法：{}", "AuthCustomerServiceImpl", "addCustomer");
                    throw new BusinessException(ResCodeEnum.ADD_FAILED, ExceptionMessageEnum.CUSTOMER_CONTACT_RELATION_ADD_FAILED);
                }
            }
        }
        /**------------------------------联系人相关------------------------------------*/

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, authCustomer.getId());
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<Integer> deleteCustomer(Integer id, AuthPlatformUserInfo userInfo) {
        AuthCustomer authCustomer = this.selectById(id);
        if (ObjectUtils.isEmpty(authCustomer) || !authCustomer.getEnterpriseId().equals(userInfo.getOrgId())) {
            return ResponseResult.buildResponseResult(ResCodeEnum.CUSTOMER_NOT_EXIST);
        }
        authCustomer.setDeleted(EnumCommon.IsDeleted.is_Delete.getKey())
                .setOperateId(userInfo.getId())
                .setDeletedTime(new Date());
        if (!this.updateById(authCustomer)) {
            log.error("删除客户失败 类{} 方法{}", "AuthCustomerServiceImpl", "deleteById");
            return ResponseResult.buildResponseResult(ResCodeEnum.DELETE_FAIL);
        }
        // 逻辑删除客户与客户分类中间表
        relationService.update(new ErpCustomerCategoryRelation()
                        .setDeleted(EnumCommon.IsDeleted.is_Delete.getKey())
                        .setDeletedTime(new Date()),
                new EntityWrapper<>(new ErpCustomerCategoryRelation().setCustomerId(authCustomer.getId())));
        // 逻辑删除客户与联系人
        customerContactService.update(new AuthCustomerContact()
                        .setDeleted(EnumCommon.IsDeleted.is_Delete.getKey())
                        .setDeletedTime(new Date()),
                new EntityWrapper<>(new AuthCustomerContact().setCustomerId(authCustomer.getId())));
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, authCustomer.getId());
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<Integer> updateCustomer(AuthCustomerUpdateRQ rq, AuthPlatformUserInfo userInfo) {
        AuthCustomer customer = customerMapper.selectOne(new AuthCustomer()
                .setId(rq.getId())
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey()));
        if (ObjectUtils.isEmpty(customer)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.CUSTOMER_NOT_EXIST);
        }
        org.springframework.beans.BeanUtils.copyProperties(rq, customer);

        customer.setUpdateTime(new Date())
                .setCusFirstType(JSON.toJSONString(rq.getCusFirstType()))
                .setCusSecondType(JSON.toJSONString(rq.getCusSecondType()));
        if (!this.updateById(customer)) {
            log.error("更新客户信息失败 类{} 方法{}", "AuthCustomerServiceImpl", "update");
            return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_FAIL);
        }
        // 更新联系人
        List<AuthContactUpdateRQ> contactList = rq.getContactList();
        if (!CollectionUtils.isEmpty(contactList)) {
            for (AuthContactUpdateRQ contactUpdateRQ : contactList) {
                contactService.updateContact(contactUpdateRQ);
            }
        }
        // 修改客户与分类关联表
        ArrayList<Integer> customerIds = Lists.newArrayList();
        customerIds.add(rq.getId());
        relationService.deleteBatchRelation(customerIds);
        relationService.addBatchRelation(rq.getId(), rq.getCusSecondType());

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, customer.getId());
    }

    /**
     * 启用/禁用客户
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<Integer> updateStatus(Integer id, Integer status, AuthPlatformUserInfo userInfo) {
        AuthCustomer authCustomer = customerMapper.selectOne(new AuthCustomer()
                .setId(id)
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey()));
        if (ObjectUtils.isEmpty(authCustomer)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.CUSTOMER_NOT_EXIST);
        }
        authCustomer.setStatus(status).setUpdateTime(new Date()).setOperateId(userInfo.getId());
        if (customerMapper.updateById(authCustomer) != 1) {
            return ResponseResult.buildResponseResult(ResCodeEnum.DELETE_FAIL);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, authCustomer.getId());
    }

    /**
     * 根据客户id查询客户详情
     */
    @Override
    public ResponseResult<AuthCustomerDetailDTO> getCustomerDetail(Integer customerId) {
        AuthCustomer customer = this.selectOne(new EntityWrapper<>(new AuthCustomer()
                .setId(customerId)
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                .setStatus(EnumCommon.IsActive.is_active.getKey())));
        if (ObjectUtils.isEmpty(customer)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.CUSTOMER_NOT_EXIST);
        }
        // 查询客户中间表
        List<AuthCustomerContact> customerContactList = customerContactService.selectList(new EntityWrapper<>(new AuthCustomerContact()
                .setCustomerId(customerId)
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                .setStatus(EnumCommon.IsActive.is_active.getKey())));
        List<Integer> contcatIds = customerContactList.stream().map(a -> a.getContactId()).collect(Collectors.toList());
        List<AuthContact> contactList = Lists.newArrayList();
        if (!CollectionUtils.isEmpty(contcatIds)) {
            contactList = contactService.selectList(new EntityWrapper<>(new AuthContact()
                    .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                    .setStatus(EnumCommon.IsActive.is_active.getKey())).in("id", contcatIds));
        }
        // 拼装返回结果
        AuthCustomerDetailDTO detailDTO = BeanUtils.copyProperties(customer, AuthCustomerDetailDTO.class);
        List<String> cusFirstType = JSON.parseArray(customer.getCusFirstType(), String.class);
        List<Integer> cusSecondType = JSON.parseArray(customer.getCusSecondType(), Integer.class);

        detailDTO.setContactList(BeanUtils.assemble(AuthContactDto.class, contactList))
                .setCusFirstType(cusFirstType)
                .setCusSecondType(cusSecondType);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, detailDTO);
    }

    private Map<String, String> getCusFirstCategoryMap() {
        List<ErpCode> customerFirstCategoryList = codeService.listErpCode(ConstantsUtil.CUSTOMER_FIRST_CATEGORY);
        return customerFirstCategoryList.stream().collect(Collectors.toMap(a -> a.getValue(), b -> b.getName()));
    }

    /**
     * 条件查询客户
     */
    @Override
    public ResponseResult<List<AuthCustomerListDto>> getList(AuthCustomerSelectRQ rq, Page page, Integer companyId) {
        Pagination pagination = PageUtils.transFromPage(page);
        EntityWrapper<AuthCustomer> wrapper = new EntityWrapper<>();
        // 构建条件
        AuthCustomer customer = new AuthCustomer().setDeleted(EnumCommon.IsDeleted.not_Delete.getKey());
        // 客户启用状态
        if (!ObjectUtils.isEmpty(rq.getStatus())) {
            customer.setStatus(rq.getStatus());
        }
        // 一级分类的条件
        if (!ObjectUtils.isEmpty(rq.getCusFirstType())) {
            wrapper.like("cus_first_type", rq.getCusFirstType());
        }
        if (!ObjectUtils.isEmpty(rq.getCusSecondType())) {
            // 二级分类不为空
            wrapper.like("cus_second_type", rq.getCusSecondType().toString());
        }
        // 客户id不为空
        if (!ObjectUtils.isEmpty(rq.getCusName())) {
            wrapper.eq("id", rq.getCusName());
        }
        List<AuthEnterpriseFlatDTO> enterpriseFlatDTOList = enterpriseFeignClient.getEnterpriseFlatByCompanyId(companyId).getObject();
        if (!CollectionUtils.isEmpty(enterpriseFlatDTOList)) {
            wrapper.in("enterprise_id", enterpriseFlatDTOList.stream().map(a -> a.getValue()).collect(Collectors.toList()));
        } else {
            customer.setEnterpriseId(companyId);
        }
        wrapper.setEntity(customer);
        wrapper.orderBy("create_time", false);
        List<AuthCustomer> customerList = customerMapper.selectPage(pagination, wrapper);

        List<AuthCustomerListDto> result = Lists.newArrayList();
        Map<String, String> map = getCusFirstCategoryMap();
        for (AuthCustomer c : customerList) {
            AuthCustomerListDto dto = BeanUtils.copyProperties(c, AuthCustomerListDto.class);
            // 获取到客户的一级和二级分类
            List<String> ones = JSON.parseArray(c.getCusFirstType(), String.class);
            List<Integer> twos = JSON.parseArray(c.getCusSecondType(), Integer.class);
            List<ErpCustomerCategory> categoryList = Lists.newArrayList();
            if (!CollectionUtils.isEmpty(twos)) {
                categoryList = customerCategoryService.selectList(new EntityWrapper<>(new ErpCustomerCategory())
                        .in("id", twos));
            }
            Set<String> firstSet = new HashSet<>();
            Set<String> twoSet = new HashSet<>();
            if (!CollectionUtils.isEmpty(ones) && ones.size() > 0) {
                ones.forEach(a -> firstSet.add(map.get(a)));
            }
            if (!CollectionUtils.isEmpty(twos) && twos.size() > 0) {
                categoryList.forEach(a -> twoSet.add(a.getName()));
            }
            dto.setCusFirstType(firstSet)
                    .setCusSecondType(twoSet);
            result.add(dto);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, result, PageUtils.transToPage(pagination));
    }

    @Override
    public ResponseResult<List<AuthCustomerBoxDto>> getEnterpriseCustomer(String pcode, AuthPlatformUserInfo userInfo) {
        Integer orgId = userInfo.getOrgId();
        EntityWrapper<AuthCustomer> wrapper = new EntityWrapper<>();
        if (!StringUtils.isBlank(pcode)) {
            wrapper.like("cus_first_type", pcode);
        }

        AuthCustomer customer = new AuthCustomer()
                .setStatus(EnumCommon.IsActive.is_active.getKey())
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey());
        List<AuthEnterpriseFlatDTO> enterpriseFlatDTOList = enterpriseFeignClient.getEnterpriseFlatByUser(userInfo.getSysToken()).getObject();
        if (!CollectionUtils.isEmpty(enterpriseFlatDTOList)) {
            wrapper.in("enterprise_id", enterpriseFlatDTOList.stream().map(a -> a.getValue()).collect(Collectors.toList()));
        } else {
            customer.setEnterpriseId(orgId);
        }
        wrapper.setEntity(customer);
        List<AuthCustomer> customerList = this.selectList(wrapper);
        List<AuthCustomerBoxDto> boxDtoList = BeanUtils.assemble(AuthCustomerBoxDto.class, customerList);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, boxDtoList);
    }

    /**
     * 根据企业id查询客户-下拉框
     */
    @Override
    public ResponseResult<List<AuthCustomerBoxDto>> getCustomerByOrgId(Integer orgId) {
        List<AuthCustomer> customerList = this.selectList(new EntityWrapper<>(new AuthCustomer()
                .setEnterpriseId(orgId)
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                .setStatus(EnumCommon.IsActive.is_active.getKey())));
        List<AuthCustomerBoxDto> boxDtoList = BeanUtils.assemble(AuthCustomerBoxDto.class, customerList);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, boxDtoList);
    }

    /**
     * 从erp码表查询客户一级分类
     */
    @Override
    public ResponseResult<List<CustomerFirstCategoryBoxDto>> getCustomerFirstCatory() {
        List<ErpCode> customerFirstCategoryList = codeService.selectList(new EntityWrapper<>(new ErpCode()
                .setCode(ConstantsUtil.CUSTOMER_FIRST_CATEGORY)));
        if (CollectionUtils.isEmpty(customerFirstCategoryList)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED, Lists.newArrayList());
        }
        List<CustomerFirstCategoryBoxDto> boxDtos = BeanUtils.assemble(CustomerFirstCategoryBoxDto.class, customerFirstCategoryList);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, boxDtos);
    }
}
