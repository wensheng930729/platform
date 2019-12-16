package com.bee.platform.dinas.datadriver.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.dinas.datadriver.dao.mapper.DinasProductMapper;
import com.bee.platform.dinas.datadriver.dto.*;
import com.bee.platform.dinas.datadriver.entity.DinasCustomerProduct;
import com.bee.platform.dinas.datadriver.entity.DinasProduct;
import com.bee.platform.dinas.datadriver.entity.DinasProductSpec;
import com.bee.platform.dinas.datadriver.rq.DinasProductAddRQ;
import com.bee.platform.dinas.datadriver.rq.DinasProductQueryRQ;
import com.bee.platform.dinas.datadriver.rq.DinasProductUpdateRQ;
import com.bee.platform.dinas.datadriver.service.DinasCustomerProductService;
import com.bee.platform.dinas.datadriver.service.DinasProductService;
import com.bee.platform.dinas.datadriver.service.DinasProductSpecService;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
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
 * 产品表 服务实现类
 * </p>
 *
 * @author liliang123
 * @since 2019-08-13
 */
@Service
public class DinasProductServiceImpl extends ServiceImpl<DinasProductMapper, DinasProduct> implements DinasProductService {

    @Autowired
    private DinasProductSpecService productSpecService;
    @Autowired
    private DinasProductMapper productMapper;
    @Autowired
    private DinasCustomerProductService customerProductService;

    /**
     * 添加产品
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<Integer> addProduct(AuthPlatformUserInfo userInfo, DinasProductAddRQ rq) {
        // 校验名称
        Integer orgId = userInfo.getOrgId();
        if (checkName(rq.getProductName(), orgId)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.PRODUCT_NAME_EXIST);
        }
        // 新数据
        DinasProduct product = BeanUtils.copyProperties(rq, DinasProduct.class);
        product.setCompanyId(orgId)
                .setDeleted(Status.FALSE.getKey())
                .setCreateTime(new Date())
                .setCreateUser(userInfo.getId());
        this.insert(product);
        // 产品和规格关联
        Integer productId = product.getId();
        List<String> rqSpecList = rq.getSpecList();
        if (!CollectionUtils.isEmpty(rqSpecList)) {
            // 去掉重复的规格
            HashSet<String> rqSpecSet = new HashSet<>(rqSpecList);
            insertProductSpec(userInfo, productId, rqSpecSet);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, product.getId());
    }

    /**
     * 检查名字是否重复
     */
    private boolean checkName(String productName, Integer orgId) {
        int count = this.selectCount(new EntityWrapper<>(new DinasProduct()
                .setCompanyId(orgId)
                .setProductName(productName)
                .setDeleted(Status.FALSE.getKey())));
        if (count > 0) {
            return true;
        }
        return false;
    }

    /**
     * 插入产品规格
     */
    private void insertProductSpec(AuthPlatformUserInfo userInfo, Integer productId, HashSet<String> rqSpecSet) {
        List<DinasProductSpec> specList = Lists.newArrayList();
        for (String specName : rqSpecSet) {
            specList.add(new DinasProductSpec()
                    .setProductId(productId)
                    .setSpecName(specName)
                    .setDeleted(Status.FALSE.getKey())
                    .setCreateTime(new Date())
                    .setCreateUser(userInfo.getId()));
        }
        productSpecService.insertBatch(specList);
    }

    /**
     * 批量删除
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<List<Integer>> deleteProduct(AuthPlatformUserInfo userInfo, List<Integer> ids) {
        // 删除产品
        DinasProduct entity = new DinasProduct().setDeleted(Status.TRUE.getKey())
                .setUpdateTime(new Date())
                .setUpdateUser(userInfo.getId());
        this.update(entity, new EntityWrapper<DinasProduct>()
                .in("id", ids)
                .eq("company_id", userInfo.getOrgId()));
        // 删除客户与产品关联表
        DinasCustomerProduct relateEntity = new DinasCustomerProduct()
                .setDeleted(Status.TRUE.getKey())
                .setUpdateTime(new Date())
                .setUpdateUser(userInfo.getId());
        customerProductService.update(relateEntity, new EntityWrapper<DinasCustomerProduct>().in("product_id", ids));
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, ids);

    }

    /**
     * 编辑
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<Integer> updateProduct(AuthPlatformUserInfo userInfo, DinasProductUpdateRQ rq) {
        Integer orgId = userInfo.getOrgId();
        // 更新
        Integer productId = rq.getId();
        DinasProduct product = this.selectOne(new EntityWrapper<>(new DinasProduct()
                .setCompanyId(orgId)
                .setId(productId)
                .setDeleted(Status.FALSE.getKey())));
        // 名称不同 校验名称
        if (!product.getProductName().equals(rq.getProductName())) {
            if (checkName(rq.getProductName(), orgId)) {
                return ResponseResult.buildResponseResult(ResCodeEnum.PRODUCT_NAME_EXIST);
            }
        }
        if (ObjectUtils.isEmpty(product)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.PRODUCT_NOT_EXIST);
        }
        product.setUpdateTime(new Date())
                .setUpdateUser(userInfo.getId());
        org.springframework.beans.BeanUtils.copyProperties(rq, product);
        this.updateById(product);
        // 更新批次信息
        productSpecService.update(new DinasProductSpec()
                        .setDeleted(Status.TRUE.getKey())
                        .setUpdateTime(new Date())
                        .setUpdateUser(userInfo.getId()),
                new EntityWrapper<>(new DinasProductSpec().setProductId(productId)));
        List<String> rqSpecList = rq.getSpecList();
        if (!CollectionUtils.isEmpty(rqSpecList)) {
            // 去掉重复的规格
            HashSet<String> rqSpecSet = new HashSet<>(rqSpecList);
            insertProductSpec(userInfo, productId, rqSpecSet);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, productId);

    }

    /**
     * 切换状态
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<Integer> switchProduct(AuthPlatformUserInfo userInfo, Integer id, Integer status) {
        DinasProduct product = this.selectOne(new EntityWrapper<>(new DinasProduct()
                .setId(id)
                .setCompanyId(userInfo.getOrgId())
                .setDeleted(Status.FALSE.getKey())));
        if (ObjectUtils.isEmpty(product)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.PRODUCT_NOT_EXIST);
        }
        if (!status.equals(product.getStatus())) {
            product.setStatus(status)
                    .setUpdateUser(userInfo.getId())
                    .setUpdateTime(new Date());
            this.updateById(product);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);
    }

    /**
     * 详情
     */
    @Override
    public ResponseResult<DinasProductDetailDTO> getProductDetail(Integer id) {
        DinasProduct product = this.selectOne(new EntityWrapper<>(new DinasProduct()
                .setId(id).setDeleted(Status.FALSE.getKey())));
        if (ObjectUtils.isEmpty(product)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.PRODUCT_NOT_EXIST);
        }
        List<DinasProductSpec> productSpecList = productSpecService.selectList(new EntityWrapper<>(new DinasProductSpec()
                .setProductId(id)
                .setDeleted(Status.FALSE.getKey())));
        DinasProductDetailDTO detailDTO = BeanUtils.copyProperties(product, DinasProductDetailDTO.class);
        detailDTO.setSpecList(BeanUtils.assemble(DinasProductSpecBoxDTO.class, productSpecList));
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, detailDTO);
    }

    /**
     * 产品列表
     */
    @Override
    public ResponseResult<List<DinasProductList2DTO>> getProductList(Integer orgId, Page page, DinasProductQueryRQ rq) {
        Pagination pagination = PageUtils.transFromPage(page);
        EntityWrapper<DinasProduct> wrapper = new EntityWrapper<>(new DinasProduct()
                .setCompanyId(orgId)
                .setDeleted(Status.FALSE.getKey()));
        wrapper.orderBy("create_time", false);
        if (!StringUtils.isBlank(rq.getProductName())) {
            wrapper.like("product_name", rq.getProductName());
        }
        if (!ObjectUtils.isEmpty(rq.getStatus())) {
            wrapper.eq("status", rq.getStatus());
        }
        List<DinasProduct> productListList = productMapper.selectPage(pagination, wrapper);
        if (CollectionUtils.isEmpty(productListList)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, new ArrayList<>(0));
        }
        List<DinasProductList2DTO> dtosList = BeanUtils.assemble(DinasProductList2DTO.class, productListList);
        Map<Integer, DinasProductList2DTO> productDtoMap = dtosList.stream().collect(Collectors.toMap(a -> a.getId(), b -> b));
        // 查询产品规格
        List<DinasProductSpec> specList = productSpecService.selectList(new EntityWrapper<DinasProductSpec>()
                .eq("deleted", Status.FALSE.getKey())
                .in("product_id", productDtoMap.keySet()));
        DinasProductList2DTO productDto;
        String specs;
        StringBuilder sb;
        for (DinasProductSpec spec : specList) {
            productDto = productDtoMap.get(spec.getProductId());
            specs = productDto.getSpecs();
            sb = new StringBuilder(specs == null ? "" : specs);
            if (StringUtils.isBlank(specs)) {
                sb.append(spec.getSpecName());
            } else {
                sb.append(",").append(spec.getSpecName());
            }
            productDto.setSpecs(sb.toString());
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dtosList, PageUtils.transToPage(pagination));
    }

    /**
     * 查询所有可关联产品
     */
    @Override
    public ResponseResult<List<DinasProductSpecAllocateDTO>> getRelateProductAll(Integer orgId) {
        List<DinasProductSpecAllocateDTO> allocateProductList = productMapper.getRelateProductAll(orgId);
        if (CollectionUtils.isEmpty(allocateProductList)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, new ArrayList<>(0));
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, allocateProductList);
    }

    /**
     * 查询客户已关联的产品-产品和规格
     */
    @Override
    public ResponseResult<List<DinasProductSpecAllocateDTO>> getRelateProductCustomer(Integer customerId) {
        List<Integer> list = new ArrayList<>(2);
        list.add(customerId);
        List<DinasProductSpecAllocateDTO> allocateProductList = productMapper.getRelateProductCustomer(list);
        if (CollectionUtils.isEmpty(allocateProductList)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, new ArrayList<>(0));
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, allocateProductList);
    }

    /**
     * 查询当前用户企业下产品
     */
    @Override
    public ResponseResult<List<DinasProductBoxDTO>> getCompanyProduct(Integer orgId) {
        List<DinasProduct> productList = this.selectList(new EntityWrapper<>(new DinasProduct()
                .setCompanyId(orgId)
                .setDeleted(Status.FALSE.getKey())
                .setStatus(Status.TRUE.getKey())));
        if (CollectionUtils.isEmpty(productList)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, new ArrayList<>(0));
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,
                BeanUtils.assemble(DinasProductBoxDTO.class, productList));
    }

    /**
     * 根据客户id查询客户关联产品
     */
    @Override
    public ResponseResult<List<DinasProductBoxDTO>> getCustomerProduct(Integer customerId) {
        List<DinasProductBoxDTO> boxDTOList = productMapper.getCustomerProduct(customerId);
        if (CollectionUtils.isEmpty(boxDTOList)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, new ArrayList<>(0));
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, boxDTOList);
    }

    /**
     * 根据客户id和产品id查询客户关联产品规格
     */
    @Override
    public ResponseResult<List<DinasProductSpecBoxDTO>> getCustomerProductSpec(Integer customerId, Integer productId) {
        HashMap<String, Integer> map = Maps.newHashMap();
        map.put("customerId", customerId);
        map.put("productId", productId);
        List<DinasProductSpecBoxDTO> boxDTOList = productMapper.getCustomerProductSpec(map);
        if (CollectionUtils.isEmpty(boxDTOList)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, new ArrayList<>(0));
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, boxDTOList);
    }

    /**
     * 根据产品id查询产品规格
     *
     * @param productId
     * @return
     */
    @Override
    public ResponseResult<List<DinasProductSpecBoxDTO>> getProductSpecByProduct(Integer productId) {
        productSpecService.selectList(new EntityWrapper<>(new DinasProductSpec()
                .setDeleted(Status.FALSE.getKey())
                .setProductId(productId)));
        return null;
    }
}
