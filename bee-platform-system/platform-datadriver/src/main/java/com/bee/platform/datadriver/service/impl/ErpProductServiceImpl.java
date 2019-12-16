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
import com.bee.platform.datadriver.dao.mapper.ErpProductMapper;
import com.bee.platform.datadriver.dto.*;
import com.bee.platform.datadriver.entity.ErpProduct;
import com.bee.platform.datadriver.entity.ErpProductCategory;
import com.bee.platform.datadriver.rq.ErpProductAddRQ;
import com.bee.platform.datadriver.rq.ErpProductListRQ;
import com.bee.platform.datadriver.rq.ErpProductUpdateRQ;
import com.bee.platform.datadriver.service.CommonService;
import com.bee.platform.datadriver.service.ErpProductService;
import com.bee.platform.user.authority.dto.AuthEnterpriseFlatDTO;
import com.bee.platform.user.service.feign.AuthEnterpriseFeignClient;
import com.google.common.collect.Lists;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * <p>
 * 产品档案 服务实现类
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@Slf4j
@Service
public class ErpProductServiceImpl extends ServiceImpl<ErpProductMapper, ErpProduct> implements ErpProductService {

    @Autowired
    private ErpProductMapper productMapper;
    @Autowired
    private ErpProductCategoryMapper productCategoryMapper;
    @Autowired
    private JedisService jedisService;
    @Autowired
    private AuthEnterpriseFeignClient enterpriseFeignClient;
    @Autowired
    private CommonService commonService;

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<Integer> addProduct(ErpProductAddRQ rq, AuthPlatformUserInfo userInfo) {
        Integer orgId = userInfo.getOrgId();
        // 判断产品名称是否重复
        List<ErpProduct> productNameCheckList = this.selectList(new EntityWrapper<>(new ErpProduct()
                .setEnterpriseId(orgId)
                .setCategory(rq.getCategory())
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                .setName(rq.getName())));
        if (!CollectionUtils.isEmpty(productNameCheckList)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.PRODUCT_NAME_EXIST);
        }
        // 复制属性
        ErpProduct product = BeanUtils.copyProperties(rq, ErpProduct.class);
        // 如果编码不为空 则校验编码是否存在
        if (!StringUtils.isEmpty(rq.getCode())) {
            ErpProduct oldProduct = productMapper.selectOne(new ErpProduct()
                    .setCode(rq.getCode())
                    .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                    .setEnterpriseId(orgId));
            if (!ObjectUtils.isEmpty(oldProduct)) {
                return ResponseResult.buildResponseResult(ResCodeEnum.CODE_EXIST);
            }
        } else {
            // 编码
            ErpProductCategory category = productCategoryMapper.selectOne(new ErpProductCategory().setId(rq.getCategory()));
            String categoryCode = category.getCode();
            String redisKey = "product" + orgId + category.getId();
            int productCode = commonService.getRedisCode(redisKey);
            product.setCode(categoryCode + commonService.getCode(productCode + "", 4));
            // redis序列自增
            jedisService.set(redisKey, (productCode + 1) + "", 0);
        }
        // 如果前端没传企业id 则默认使用当前登录人企业id
        if (ObjectUtils.isEmpty(rq.getEnterpriseId())) {
            product.setEnterpriseId(orgId);
        }
        product.setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                .setCheckItems(JSON.toJSONString(rq.getCheckItems()))
                .setLogo(JSON.toJSONString(rq.getLogo()))
                .setCreateTime(new Date())
                .setOperateId(userInfo.getId());
        if (productMapper.insert(product) != 1) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ADD_FAILED);
        }

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, product.getId());
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<Integer> deleteProduct(Integer id, AuthPlatformUserInfo userInfo) {
        ErpProduct product = productMapper.selectOne(new ErpProduct().setId(id).setEnterpriseId(userInfo.getOrgId()));
        if (ObjectUtils.isEmpty(product)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.PRODUCT_NOT_EXIST);
        }
        product.setOperateId(userInfo.getId())
                .setDeletedTime(new Date())
                .setDeleted(EnumCommon.IsDeleted.is_Delete.getKey());
        if (productMapper.updateById(product) != 1) {
            return ResponseResult.buildResponseResult(ResCodeEnum.DELETE_FAIL);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, product.getId());
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<Integer> updateProduct(ErpProductUpdateRQ rq, AuthPlatformUserInfo userInfo) {
        Integer orgId = userInfo.getOrgId();
        ErpProduct product = this.selectOne(new EntityWrapper<>(new ErpProduct().setId(rq.getId()).setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())));
        // 如果修改了名称 则判断名称是否重复
        if (!rq.getName().equals(product.getName())) {
            // 判断产品名称是否重复
            List<ErpProduct> productNameCheckList = this.selectList(new EntityWrapper<>(new ErpProduct()
                    .setEnterpriseId(orgId)
                    .setCategory(rq.getCategory())
                    .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                    .setName(rq.getName())));
            if (!CollectionUtils.isEmpty(productNameCheckList)) {
                return ResponseResult.buildResponseResult(ResCodeEnum.PRODUCT_NAME_EXIST);
            }
        }
        // 如果修改过编码 则校验是否重复
        if (!rq.getCode().equals(product.getCode())) {
            List<ErpProduct> oldProducts = this.selectList(new EntityWrapper<>(new ErpProduct()
                    .setCode(rq.getCode())
                    .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                    .setEnterpriseId(orgId)));
            if (!CollectionUtils.isEmpty(oldProducts)) {
                return ResponseResult.buildResponseResult(ResCodeEnum.CODE_EXIST);
            }
        }
        org.springframework.beans.BeanUtils.copyProperties(rq, product);
        product.setOperateId(orgId)
                .setUpdateTime(new Date())
                .setLogo(JSON.toJSONString(rq.getLogo()))
                .setCheckItems(JSON.toJSONString(rq.getCheckItems()));
        if (productMapper.updateById(product) != 1) {
            return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_FAIL);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, product.getId());
    }

    /**
     * 禁用/启用产品
     */
    @Override
    public ResponseResult<Integer> updateStatus(Integer id, Integer status, AuthPlatformUserInfo userInfo) {
        ErpProduct product = this.selectOne(new EntityWrapper<>(new ErpProduct().setId(id)
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())));
        if (ObjectUtils.isEmpty(product)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.PRODUCT_NOT_EXIST);
        }
        product.setStatus(status).setUpdateTime(new Date()).setOperateId(userInfo.getId());
        if (!this.updateById(product)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_FAIL);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, product.getId());
    }

    @Override
    public ResponseResult<List<ErpProductListDTO>> getProductConditional(ErpProductListRQ rq, Page page, Integer companyId) {
        Pagination pagination = PageUtils.transFromPage(page);
        // 构建条件
        ErpProduct erpProduct = new ErpProduct()
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                .setEnterpriseId(companyId);
        if (!ObjectUtils.isEmpty(rq)) {
            Optional<ErpProductListRQ> optional = Optional.of(rq);
            erpProduct.setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                    .setName(optional.map(a -> a.getName()).orElse(null))
                    .setCategory(optional.map(a -> a.getCategory()).orElse(null))
                    .setStatus(optional.map(a -> a.getStatus()).orElse(null));
        }
        List<ErpProduct> products = productMapper.selectPage(pagination, new EntityWrapper<>(erpProduct)
                .orderBy("create_time", false));
        List<ErpProductListDTO> productDTOS = BeanUtils.assemble(ErpProductListDTO.class, products);
        // 查询产品类别名称
        List<ErpProductCategory> productCategoryList = productCategoryMapper.selectList(new EntityWrapper<>(new ErpProductCategory()
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                .setStatus(EnumCommon.IsActive.is_active.getKey())
                .setEnterpriseId(companyId)));
        Map<Integer, String> categoryMap = productCategoryList.stream().collect(Collectors.toMap(a -> a.getId(), b -> b.getName()));
        for (ErpProductListDTO dto : productDTOS) {
            String categoryName = categoryMap.get(dto.getCategory());
            dto.setCategoryName(categoryName == null ? "" : categoryName);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, productDTOS, PageUtils.transToPage(pagination));
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ErpProductDetailDTO> getById(Integer id) {
        ErpProduct product = this.selectOne(new EntityWrapper<>(new ErpProduct()
                .setId(id)
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())));
        if (ObjectUtils.isEmpty(product)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.PRODUCT_NOT_EXIST);
        }
        ErpProductDetailDTO productDetailDTO = BeanUtils.copyProperties(product, ErpProductDetailDTO.class);
        if (product.getLogo() != null) {
            productDetailDTO.setLogo(JSON.parseArray(product.getLogo(), ErpLogoDTO.class));
        }
        if (product.getCheckItems() != null) {
            productDetailDTO.setCheckItems(JSON.parseArray(product.getCheckItems(), ErpProductCategoryCheckItemsDTO.class));
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, productDetailDTO);
    }

    /**
     * 当前用户所在企业的产品
     */
    @Override
    public ResponseResult<List<ErpProductBoxDTO>> getEnterpriseProduct(AuthPlatformUserInfo userInfo) {
        List<Integer> ids = Lists.newArrayList();
        ids.add(userInfo.getOrgId());
        return getProduct(ids, userInfo);
    }

    private ResponseResult<List<ErpProductBoxDTO>> getProduct(List<Integer> ids, AuthPlatformUserInfo userInfo) {
        List<ErpProduct> products = selectList(new EntityWrapper<>(new ErpProduct()
                .setEnterpriseId(userInfo.getOrgId())
                .setStatus(EnumCommon.IsActive.is_active.getKey())
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey()))
                .in("enterprise_id", ids));
        List<ErpProductBoxDTO> boxDTOS = BeanUtils.assemble(ErpProductBoxDTO.class, products);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, boxDTOS);
    }

    /**
     * 当前用户所在企业及-子企业的产品
     */
    @Override
    public ResponseResult<List<ErpProductBoxDTO>> getEnterpriseSubProduct(AuthPlatformUserInfo userInfo) {
        List<AuthEnterpriseFlatDTO> enterpriseFlatDTOS = enterpriseFeignClient.getEnterpriseFlatByUser(userInfo.getSysToken()).getObject();
        List<Integer> ids = null;
        if (CollectionUtils.isEmpty(enterpriseFlatDTOS)) {
            ids = Lists.newArrayList();
            ids.add(userInfo.getOrgId());
        } else {
            ids = enterpriseFlatDTOS.stream().map(a -> a.getValue()).collect(Collectors.toList());
        }
        return getProduct(ids, userInfo);
    }

    /**
     * 当前用户所在企业及-集团下所有公司的产品
     */
    @Override
    public ResponseResult<List<ErpProductBoxDTO>> getEnterpriseGroupProduct(AuthPlatformUserInfo userInfo) {
        List<AuthEnterpriseFlatDTO> enterpriseFlatDTOS = enterpriseFeignClient.getParentSubEnterpriseFlat(userInfo.getSysToken()).getObject();
        List<Integer> ids = null;
        if (CollectionUtils.isEmpty(enterpriseFlatDTOS)) {
            ids = Lists.newArrayList();
            ids.add(userInfo.getOrgId());
        } else {
            ids = enterpriseFlatDTOS.stream().map(a -> a.getValue()).collect(Collectors.toList());
        }
        return getProduct(ids, userInfo);
    }

    /**
     * 通过产品id获取产品名称和产品分类名称
     */
    @Override
    public ErpProductCategoryNameDTO getProductCategoryName(Integer productId) {
        return baseMapper.getProductCategory(productId);
    }

    /**
     * 根据产品id获取 该产品的检测属性
     */
    @Override
    public ResponseResult<List<ErpProductCategoryCheckItemsDTO>> getCheckItems(Integer id) {
        ErpProduct product = this.selectOne(new EntityWrapper<>(new ErpProduct().setId(id)));
        if (ObjectUtils.isEmpty(product)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.PRODUCT_NOT_EXIST);
        }
        List<ErpProductCategoryCheckItemsDTO> checkItems = JSON.parseArray(product.getCheckItems(), ErpProductCategoryCheckItemsDTO.class);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,
                checkItems.stream().filter(a -> a.getStatus().equals(EnumCommon.IsActive.is_active.getKey())).collect(Collectors.toList()));
    }
}
