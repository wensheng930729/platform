package com.bee.platform.customer.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.constants.enums.EnumCommon;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.customer.dao.mapper.ErpCustomerCategoryMapper;
import com.bee.platform.customer.dto.ErpCustomerCategoryBoxDTO;
import com.bee.platform.customer.dto.ErpCustomerCategoryListDTO;
import com.bee.platform.customer.entity.ErpCustomerCategory;
import com.bee.platform.customer.rq.ErpCustomerCategoryAddRQ;
import com.bee.platform.customer.rq.ErpCustomerCategorySelectRQ;
import com.bee.platform.customer.rq.ErpCustomerCategoryUpdateRQ;
import com.bee.platform.customer.service.ErpCustomerCategoryService;
import com.bee.platform.datadriver.entity.ErpCode;
import com.bee.platform.datadriver.service.ErpCodeService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

import java.util.Date;
import java.util.List;

/**
 * <p>
 * 客户分类 服务实现类
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@Service
public class ErpCustomerCategoryServiceImpl extends ServiceImpl<ErpCustomerCategoryMapper, ErpCustomerCategory> implements ErpCustomerCategoryService {

    @Autowired
    private ErpCustomerCategoryMapper customerCategoryMapper;
    @Autowired
    private ErpCodeService codeService;

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<Integer> addCustomerCategory(ErpCustomerCategoryAddRQ rq, AuthPlatformUserInfo userInfo) {
        // 名称查重
        List<ErpCustomerCategory> categoryList = customerCategoryMapper.selectList(new EntityWrapper<>(new ErpCustomerCategory()
                .setEnterpriseId(userInfo.getOrgId())
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                .setPcode(rq.getPcode())
                .setName(rq.getName())));
        if (!CollectionUtils.isEmpty(categoryList)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.CATEGORY_EXIST);
        }
        // 查询码表获取一级分类名称
        ErpCode sysCode = codeService.selectOne(new EntityWrapper<>(new ErpCode()
                .setValue(rq.getPcode())
                .setCode("customer_category_first")));
        if (ObjectUtils.isEmpty(sysCode)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_GET_FIRST_CUSTOMER_CATEGORY);
        }
        ErpCustomerCategory category = BeanUtils.copyProperties(rq, ErpCustomerCategory.class)
//                .setPCode(sysCode.getValue())
                .setPName(sysCode.getName())
                .setCreateTime(new Date())
                .setOperateId(userInfo.getId())
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                .setEnterpriseId(userInfo.getOrgId())
                .setUpdateTime(new Date());
        this.insert(category);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, category.getId());
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<Integer> deleteCustomerCategory(Integer id, AuthPlatformUserInfo userInfo) {
        ErpCustomerCategory category = selectById(id);
        if (ObjectUtils.isEmpty(category)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.CUSTOMER_CATEGORY_NOT_EXIST);
        }
        category.setDeleted(EnumCommon.IsDeleted.is_Delete.getKey()).setDeletedTime(new Date()).setOperateId(userInfo.getId());
        this.updateById(category);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, category.getId());
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<Integer> updateCustomerCategory(ErpCustomerCategoryUpdateRQ rq, AuthPlatformUserInfo userInfo) {
        // 查看记录存在？
        ErpCustomerCategory category = selectById(rq.getId());
        if (ObjectUtils.isEmpty(category)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.CUSTOMER_CATEGORY_NOT_EXIST);
        }

        if (!rq.getName().equals(category.getName())) {
            // 查重
            List<ErpCustomerCategory> categoryList = customerCategoryMapper.selectList(new EntityWrapper<>(new ErpCustomerCategory()
                    .setEnterpriseId(userInfo.getOrgId())
                    .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                    .setPcode(rq.getPcode())
                    .setName(rq.getName())));
            if (!CollectionUtils.isEmpty(categoryList)) {
                return ResponseResult.buildResponseResult(ResCodeEnum.NAME_EXIST);
            }
        }
        // 查询码表获取一级分类名称
        ErpCode sysCode = codeService.selectOne(new EntityWrapper<>(new ErpCode()
                .setValue(rq.getPcode())
                .setDescription("客户分类一级分类")));
        org.springframework.beans.BeanUtils.copyProperties(rq, category);
        category.setUpdateTime(new Date())
                .setPName(sysCode.getName());
        this.updateById(category);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, category.getId());
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<Integer> updateSatatus(Integer id, Integer status, AuthPlatformUserInfo userInfo) {
        ErpCustomerCategory category = customerCategoryMapper.selectOne(new ErpCustomerCategory()
                .setId(id)
                .setEnterpriseId(userInfo.getOrgId())
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey()));
        if (ObjectUtils.isEmpty(category)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.CUSTOMER_CATEGORY_NOT_EXIST);
        }
        category.setStatus(status).setUpdateTime(new Date()).setOperateId(userInfo.getId());
        customerCategoryMapper.updateById(category);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, category.getId());
    }

    @Override
    public ResponseResult<List<ErpCustomerCategoryListDTO>> getList(ErpCustomerCategorySelectRQ rq, Integer companyId, Page page) {
        Pagination pagination = PageUtils.transFromPage(page);
        ErpCustomerCategory category = new ErpCustomerCategory()
                .setEnterpriseId(companyId)
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey());
        if (!StringUtils.isEmpty(rq.getPcode())) {
            category.setPcode(rq.getPcode());
        }
        if (!ObjectUtils.isEmpty(rq.getId())) {
            category.setId(rq.getId());
        }
        if (!ObjectUtils.isEmpty(rq.getStatus())) {
            category.setStatus(rq.getStatus());
        }
        List<ErpCustomerCategory> categories = customerCategoryMapper.selectPage(pagination, new EntityWrapper<>(category)
                .orderBy("create_time", false));
        List<ErpCustomerCategoryListDTO> listDTOS = BeanUtils.assemble(ErpCustomerCategoryListDTO.class, categories);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, listDTOS, PageUtils.transToPage(pagination));
    }

    @Override
    public ResponseResult getEnterpriseTwoCategories(String pcode, AuthPlatformUserInfo userInfo) {
        ErpCustomerCategory category = new ErpCustomerCategory()
                .setEnterpriseId(userInfo.getOrgId())
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                .setStatus(EnumCommon.IsActive.is_active.getKey())
                .setPcode(pcode);
        List<ErpCustomerCategory> categoryList = this.selectList(new EntityWrapper<>(category));
        List<ErpCustomerCategoryBoxDTO> boxDTOList = BeanUtils.assemble(ErpCustomerCategoryBoxDTO.class, categoryList);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, boxDTOList);
    }

    /**
     * 通过登录用户企业下的二级客户分类-下拉框使用
     *
     * @param userInfo
     * @param isList     是否是客户分类列表使用
     *                 如果是则需要查出禁用的二级分类 否 则不需要
     * @return
     */
    @Override
    public ResponseResult<List<ErpCustomerCategoryBoxDTO>> getUserTwoCategories(AuthPlatformUserInfo userInfo, boolean isList) {
        ErpCustomerCategory category = new ErpCustomerCategory()
                .setEnterpriseId(userInfo.getOrgId())
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey());
        if (!isList) {
            category.setStatus(EnumCommon.IsActive.is_active.getKey());
        }
        List<ErpCustomerCategory> categoryList = this.selectList(new EntityWrapper<>(category));
        List<ErpCustomerCategoryBoxDTO> boxDTOList = BeanUtils.assemble(ErpCustomerCategoryBoxDTO.class, categoryList);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, boxDTOList);
    }
}
