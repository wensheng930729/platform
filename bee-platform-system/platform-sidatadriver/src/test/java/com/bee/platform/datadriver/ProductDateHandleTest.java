package com.bee.platform.datadriver;


import com.alibaba.fastjson.JSON;
import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.bee.platform.Application;
import com.bee.platform.datadriver.dto.ErpProductCheckItemsDTO;
import com.bee.platform.datadriver.dto.ErpProductCheckItemsOriginalDTO;
import com.bee.platform.datadriver.entity.ErpProduct;
import com.bee.platform.datadriver.entity.ErpProductCategory;
import com.bee.platform.datadriver.service.ErpProductCategoryService;
import com.bee.platform.datadriver.service.ErpProductService;
import com.google.common.collect.Lists;
import lombok.extern.slf4j.Slf4j;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * @ClassName: NewTest
 * @Description: java类作用描述
 * @Author: fei.sun
 * @Date: 2019/4/25 15:45
 * @Version: 1.0
 */

@Slf4j
@SpringBootTest(classes = {Application.class})
@RunWith(SpringRunner.class)
public class ProductDateHandleTest {

    @Autowired
    private ErpProductService productservice;
    @Autowired
    private ErpProductCategoryService productCategoryService;

    @Test
    public void test() {
        List<ErpProduct> productList = productservice.selectList(new EntityWrapper<ErpProduct>());
        // 产品分类的map
        List<ErpProductCategory> productCategoryList = productCategoryService.selectList(new EntityWrapper<ErpProductCategory>());
        Map<Integer, String> productCategoryMap = productCategoryList.stream().collect(Collectors.toMap(a -> a.getId(), b -> b.getName()));

        int size = productList.size();
        for (int i = 0; i < size; i++) {
            ErpProduct product = productList.get(i);
            int count = 0;
            // 检测属性
            List<ErpProductCheckItemsOriginalDTO> originalDTOList = JSON.parseArray(product.getCheckItems(), ErpProductCheckItemsOriginalDTO.class);
            List<ErpProductCheckItemsDTO> checkItemsDTOList = Lists.newArrayList();
            for (ErpProductCheckItemsOriginalDTO originalDTO : originalDTOList) {
                if (originalDTO.getShow().equals(1)) {
                    count++;
                }
                ErpProductCheckItemsDTO checkItemsDTO = new ErpProductCheckItemsDTO().setName(originalDTO.getName());
                // 主属性最多选择2个
                if (count >= 2) {
                    checkItemsDTO.setShow(0);
                } else {
                    checkItemsDTO.setShow(originalDTO.getShow());
                }
                checkItemsDTOList.add(checkItemsDTO);
            }
            product.setCheckItems(JSON.toJSONString(checkItemsDTOList));
            // 产品类别
            if ("主料".equals(productCategoryMap.get(product.getCategory()))) {
                product.setCategory(1);
            } else if ("辅料".equals(productCategoryMap.get(product.getCategory()))) {
                product.setCategory(2);
            } else if ("成品".equals(productCategoryMap.get(product.getCategory()))) {
                product.setCategory(3);
            } else {
                product.setCategory(4);
            }
        }
        productservice.updateBatchById(productList);
    }
}
