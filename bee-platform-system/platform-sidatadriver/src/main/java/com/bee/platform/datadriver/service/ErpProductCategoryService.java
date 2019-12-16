package com.bee.platform.datadriver.service;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.datadriver.dto.ErpProductCategoryDTO;
import com.bee.platform.datadriver.entity.ErpProductCategory;

import java.util.List;

/**
 * <p>
 * 产品类别 服务类
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
public interface ErpProductCategoryService extends IService<ErpProductCategory> {

//    /**
//     * 添加产品分类
//     *
//     * @param rq
//     * @return
//     */
//    ResponseResult<Integer> addCategory(ErpProductCategoryAddRQ rq, AuthPlatformUserInfo userInfo);
//
//    /**
//     * 更新erp产品分类
//     *
//     * @param rq
//     * @return
//     */
//    ResponseResult<Integer> updateCategory(ErpProductCategoryUpdateRQ rq, AuthPlatformUserInfo userInfo);
//
//    /**
//     * 禁用/启用产品分类
//     *
//     * @param id
//     * @param status
//     * @return
//     */
//    ResponseResult<Integer> updateStatus(Integer id, Integer status, AuthPlatformUserInfo userInfo);
//
//    /**
//     * 删除erp产品分类
//     *
//     * @param id 分类id
//     * @return
//     */
//    ResponseResult<Integer> deleteCategory(Integer id, AuthPlatformUserInfo userInfo);
//
//    /**
//     * 根据条件查询产品分类 列表
//     *
//     * @param rq
//     * @return
//     */
//    ResponseResult getCategoryConditional(ErpProductCategoryListRQ rq, Page page, Integer companyId);
//
//    /**
//     * 通过id获取 产品分类详情
//     *
//     * @param id
//     * @return
//     */
//    ResponseResult getById(Integer id);

    /**
     * 获取当前登录人所在企业所有的产品分类
     *
     * @return
     */
    ResponseResult<List<ErpProductCategoryDTO>> getCategory();

//    /**
//     * 根据产品类别id获取 该企业下产品分类的检测属性
//     *
//     * @param id 产品分类id
//     * @return
//     */
//    ResponseResult<List<ErpProductCheckItemsDTO>> getCheckItems(Integer id);

}
