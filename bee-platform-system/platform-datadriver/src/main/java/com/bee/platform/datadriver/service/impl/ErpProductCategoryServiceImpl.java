package com.bee.platform.datadriver.service.impl;

import com.alibaba.fastjson.JSON;
import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.constants.enums.EnumCommon;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.service.JedisService;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.datadriver.dao.mapper.ErpProductCategoryMapper;
import com.bee.platform.datadriver.dto.ErpProductCategoryCheckItemsDTO;
import com.bee.platform.datadriver.dto.ErpProductCategoryDTO;
import com.bee.platform.datadriver.dto.ErpProductCategoryDetailDTO;
import com.bee.platform.datadriver.dto.ErpProductCategoryListDTO;
import com.bee.platform.datadriver.entity.ErpProductCategory;
import com.bee.platform.datadriver.rq.ErpProductCategoryAddRQ;
import com.bee.platform.datadriver.rq.ErpProductCategoryListRQ;
import com.bee.platform.datadriver.rq.ErpProductCategoryUpdateRQ;
import com.bee.platform.datadriver.service.CommonService;
import com.bee.platform.datadriver.service.ErpProductCategoryService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

import java.util.Date;
import java.util.List;
import java.util.Optional;

/**
 * <p>
 * 产品类别 服务实现类
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@Service
public class ErpProductCategoryServiceImpl extends ServiceImpl<ErpProductCategoryMapper, ErpProductCategory> implements ErpProductCategoryService {

    @Autowired
    private ErpProductCategoryMapper productCategoryMapper;
    @Autowired
    private JedisService jedisService;
    @Autowired
    private CommonService commonService;

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<Integer> addCategory(ErpProductCategoryAddRQ rq, AuthPlatformUserInfo userInfo) {
        Integer orgId = userInfo.getOrgId();
        // 判断产品分类名称是否重复
        int categoryNameCheck = this.selectCount(new EntityWrapper<>(new ErpProductCategory()
                .setEnterpriseId(orgId)
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                .setName(rq.getName())));
        if (categoryNameCheck > 0) {
            return ResponseResult.buildResponseResult(ResCodeEnum.PRODUCT_CATEGORY_NAME_EXIST);
        }

        ErpProductCategory productCategory = BeanUtils.copyProperties(rq, ErpProductCategory.class);
        // 如果编码不为空 则校验编码是否存在
        if (!StringUtils.isEmpty(rq.getCode())) {
            ErpProductCategory oldProductCategory = productCategoryMapper.selectOne(new ErpProductCategory()
                    .setCode(rq.getCode())
                    .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                    .setEnterpriseId(orgId));
            if (!ObjectUtils.isEmpty(oldProductCategory)) {
                return ResponseResult.buildResponseResult(ResCodeEnum.CODE_EXIST);
            }
        } else {
            // 手动生成编码
            String redisKey = "productCategory" + orgId;
            int productCategoryCode = commonService.getRedisCode(redisKey);
            productCategory.setCode(commonService.getCode(productCategoryCode + "", 2));
            // redis序号自增
            jedisService.set(redisKey, (productCategoryCode + 1) + "", 0);
        }
        String checkItems = JSON.toJSONString(rq.getCheckItems());
        productCategory.setOperateId(userInfo.getId())
                .setEnterpriseId(orgId)
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                .setCreateTime(new Date())
                .setCheckItems(checkItems);
        if (productCategoryMapper.insert(productCategory) != 1) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ADD_FAILED);
        }

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, productCategory.getId());
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<Integer> updateCategory(ErpProductCategoryUpdateRQ rq, AuthPlatformUserInfo userInfo) {
        Integer orgId = userInfo.getOrgId();
        ErpProductCategory productCategory = productCategoryMapper.selectOne(new ErpProductCategory().setId(rq.getId()));
        if (ObjectUtils.isEmpty(productCategory)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.PRODUCT_CATEGORY_NOT_EXIST);
        }
        // 如果修改了分类编号 则判断编码是否重复
        if (!rq.getCode().equals(productCategory.getCode())) {
            List<ErpProductCategory> categoryCodeCheckList = this.selectList(new EntityWrapper<>(new ErpProductCategory()
                    .setEnterpriseId(orgId)
                    .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                    .setCode(rq.getCode())));
            if (!CollectionUtils.isEmpty(categoryCodeCheckList)) {
                return ResponseResult.buildResponseResult(ResCodeEnum.CODE_EXIST);
            }
        }
        // 如果修改了分类名称 则判断名称是否重复
        if (!rq.getName().equals(productCategory.getName())) {
            List<ErpProductCategory> categoryNameCheckList = this.selectList(new EntityWrapper<>(new ErpProductCategory()
                    .setEnterpriseId(orgId)
                    .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                    .setName(rq.getName())));
            if (!CollectionUtils.isEmpty(categoryNameCheckList)) {
                return ResponseResult.buildResponseResult(ResCodeEnum.PRODUCT_CATEGORY_NAME_EXIST);
            }
        }
        org.springframework.beans.BeanUtils.copyProperties(rq, productCategory);
        productCategory.setCheckItems(JSON.toJSONString(rq.getCheckItems()))
                .setOperateId(userInfo.getId())
                .setUpdateTime(new Date());
        if (productCategoryMapper.updateById(productCategory) != 1) {
            return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_FAIL);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, productCategory.getId());
    }

    @Override
    public ResponseResult<Integer> updateStatus(Integer id, Integer status, AuthPlatformUserInfo userInfo) {
        ErpProductCategory productCategory = this.selectOne(new EntityWrapper<>(new ErpProductCategory().setId(id)
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())));
        if (ObjectUtils.isEmpty(productCategory)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.PRODUCT_CATEGORY_NOT_EXIST);
        }
        productCategory.setStatus(status).setUpdateTime(new Date()).setOperateId(userInfo.getId());
        if (!this.updateById(productCategory)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_FAIL);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, productCategory.getId());
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<Integer> deleteCategory(Integer id, AuthPlatformUserInfo userInfo) {
        ErpProductCategory productCategory = productCategoryMapper.selectOne(new ErpProductCategory().setId(id));
        if (ObjectUtils.isEmpty(productCategory)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.PRODUCT_CATEGORY_NOT_EXIST);
        }
        productCategory.setDeleted(EnumCommon.IsDeleted.is_Delete.getKey())
                .setDeletedTime(new Date())
                .setOperateId(userInfo.getId());
        if (productCategoryMapper.updateById(productCategory) != 1) {
            return ResponseResult.buildResponseResult(ResCodeEnum.DELETE_FAIL);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, productCategory.getId());
    }

    @Override
    public ResponseResult getCategoryConditional(ErpProductCategoryListRQ rq, Page page, Integer companyId) {
        Pagination pagination = PageUtils.transFromPage(page);
        ErpProductCategory productCategory = new ErpProductCategory()
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                .setEnterpriseId(companyId);
        if (!ObjectUtils.isEmpty(rq)) {
            Optional<ErpProductCategoryListRQ> optional = Optional.of(rq);
            productCategory.setName(optional.map(a -> a.getName()).orElse(null));
            productCategory.setStatus(optional.map(a -> a.getStatus()).orElse(null));
        }
        List<ErpProductCategory> categories = productCategoryMapper.selectPage(pagination,
                new EntityWrapper<>(productCategory)
                        .orderBy("create_time", false));
        List<ErpProductCategoryListDTO> categoryListDTOS = BeanUtils.assemble(ErpProductCategoryListDTO.class, categories);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, categoryListDTOS, PageUtils.transToPage(pagination));
    }

    @Override
    public ResponseResult getById(Integer id) {
        ErpProductCategory productCategory = productCategoryMapper.selectOne(new ErpProductCategory().setId(id));
        if (ObjectUtils.isEmpty(productCategory)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.PRODUCT_CATEGORY_NOT_EXIST);
        }
        ErpProductCategoryDetailDTO detailDTO = BeanUtils.copyProperties(productCategory, ErpProductCategoryDetailDTO.class);
        detailDTO.setCheckItems(JSON.parseArray(productCategory.getCheckItems(), ErpProductCategoryCheckItemsDTO.class));
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, detailDTO);
    }

    @Override
    public ResponseResult<List<ErpProductCategoryDTO>> getCategory(AuthPlatformUserInfo userInfo) {
        List<ErpProductCategory> categories = productCategoryMapper.selectList(new EntityWrapper<>(new ErpProductCategory()
                .setEnterpriseId(userInfo.getOrgId())
                .setStatus(EnumCommon.IsActive.is_active.getKey())
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())));
        List<ErpProductCategoryDTO> dtos = BeanUtils.assemble(ErpProductCategoryDTO.class, categories);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dtos);
    }

    @Override
    public ResponseResult<List<ErpProductCategoryCheckItemsDTO>> getCheckItems(Integer id) {
        ErpProductCategory productCategory = productCategoryMapper.selectOne(new ErpProductCategory().setId(id));
        if (ObjectUtils.isEmpty(productCategory)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.PRODUCT_CATEGORY_NOT_EXIST);
        }
        List<ErpProductCategoryCheckItemsDTO> checkItems = JSON.parseArray(productCategory.getCheckItems(), ErpProductCategoryCheckItemsDTO.class);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, checkItems);
    }
}
