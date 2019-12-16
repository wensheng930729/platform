package com.bee.platform.user;

/**
 * @description:
 * @author: junyang.li
 * @create: 2019-01-07 16:47
 **/

import cn.hutool.poi.excel.ExcelFileUtil;
import cn.hutool.poi.excel.ExcelPicUtil;
import cn.hutool.poi.excel.ExcelUtil;
import cn.hutool.poi.excel.ExcelWriter;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.bee.platform.common.config.RestTemplateConfig;
import com.bee.platform.common.enums.MResourceLev;
import com.bee.platform.common.enums.MResourceType;
import com.bee.platform.common.service.JedisService;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.user.authority.dao.mapper.AuthResourceMapper;
import com.bee.platform.user.authority.entity.AuthResource;
import com.bee.platform.user.authority.service.AuthResourceService;
import com.bee.platform.user.dao.mapper.MResourceMapper;
import com.bee.platform.user.dao.mapper.MRoleResourceMapper;
import com.bee.platform.user.dto.MResourceDTO;
import com.bee.platform.user.entity.MResource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang.StringEscapeUtils;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.util.CollectionUtils;

import java.io.*;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@Slf4j
@RunWith(SpringRunner.class)
@SpringBootTest(classes={Application.class})
@SuppressWarnings("all")
public class ReadFileTest {

    @Autowired
    private MResourceMapper resourceMapper;

    @Autowired
    private JedisService jedisService;

    @Autowired
    private MRoleResourceMapper roleMenuMapper;


    private static Pattern p = Pattern.compile("\\s*|\t|\r|\n");

    @Autowired
    private RestTemplateConfig restTemplate;

    @Autowired
    private AuthResourceService authResourceService;

    @Autowired
    private AuthResourceMapper authResourceMapper;


    private String readFile(String path){
        try {
            String encoding = "UTF-8";
            File file = new File(path);
            if (file.isFile() && file.exists()) { //判断文件是否存在
                InputStreamReader read = new InputStreamReader(
                        new FileInputStream(file), encoding);//考虑到编码格式
                BufferedReader bufferedReader = new BufferedReader(read);
                String lineTxt = null;
                StringBuilder builder = new StringBuilder();
                while ((lineTxt = bufferedReader.readLine()) != null) {
                    builder.append(lineTxt);
                    System.out.println(lineTxt);
                }
                read.close();
                String str = StringEscapeUtils.unescapeJava(builder.toString());
                Matcher m = p.matcher(str);
                str = m.replaceAll("");
                return str;
            }
        }catch (IOException e){
            e.printStackTrace();
        }
        return null;
    }


    /**
     * @notes 读取本地json文件，并存入数据库中
     * @Author junyang.li
     * @Date 16:47 2019/1/7
     **/
    @Test
    public void read() throws IOException {
        String str=readFile("F:\\interface.txt");
        if(str==null){
            System.out.println("文件不存在");
            return;
        }
        JSONArray json=JSONArray.parseArray(str);
        if(json==null){
            return;
        }
        List<MResourceDTO> list=json.toJavaList(MResourceDTO.class);
        //转换数据
        List<AuthResource> all=new ArrayList<>();
        //递归获得所有的子菜单
        recursion(list,all,0);
        //resourceMapper.delete(new EntityWrapper<MResource>().where("status=1"));
        //插入所有菜单资源
        //authResourceMapper.insertAll(all);
        Workbook workbook=ExcelUtil.getBigWriter().write(all).getWorkbook();
        OutputStream out=new FileOutputStream("F:\\interface.xls");
        workbook.write(out);
        //jedisService.delKey(ConstantsUtil.SYSTEM_ALL_ROLE);
        //addRoleMenu(fristMenu,Arrays.asList(new Integer[]{5}));
    }

    private static int num=1000;

    /**
     * @notes 递归将所有的子菜单取出来，放在一个list对象中，然后插入数据库
     * @Author junyang.li
     * @Date 18:34 2019/1/7
     **/
    private void recursion(List<MResourceDTO> list,List<AuthResource> all,int parentId){
        for (int k = 0; k < list.size(); k++) {
            MResourceDTO item=list.get(k);
            AuthResource childen= create(item,num,parentId);
            num++;
            all.add(childen);
            List<MResourceDTO> me=item.getRoutes();
            if(!CollectionUtils.isEmpty(me)){
                recursion(me,all,childen.getId());
            }
        }
    }


    private AuthResource create(MResourceDTO obj,int t,int c){
        AuthResource resource=new AuthResource().setId(t).setPid(c).setSubSys("bee_platform").setName(obj.getName()==null?"":obj.getName())
                .setType(0).setIcon(obj.getIcon()).setPath(obj.getPath()).setComponent(obj.getComponent()).setDeleted(0)
                .setCreateTime(new Date()).setUpdateTime(new Date());
        if(obj.getHideChildrenInMenu()==null){
            resource.setHide(0);
        }else {
            resource.setHide(obj.getHideChildrenInMenu()?1:0);
        }
        return resource;
    }


    @Test
    public void analysisInterface() throws IOException {
        JSONObject json=restTemplate.sendRestGet("http://localhost:8075/v2/api-docs",null);
        if(json==null){
            System.out.println("文件不存在");
            return;
        }
        JSONObject paths=json.getJSONObject("paths");
        Map<String,JSONObject> map=paths.toJavaObject(Map.class);
        Workbook workbook=new XSSFWorkbook();
        Sheet sheet=workbook.createSheet();
        //从0行开始
        int index=0;
        //从0列开始
        int cell=0;
        for (Map.Entry<String, JSONObject> entry:map.entrySet()){
            Row row=sheet.createRow(index);
            row.createCell(cell).setCellValue(replace(entry.getKey()));
            cell++;
            Map<String,JSONObject> map1=entry.getValue().toJavaObject(Map.class);
            for (Map.Entry<String, JSONObject> item:map1.entrySet()){
                row.createCell(cell).setCellValue(item.getKey().toUpperCase());
                cell++;
                row.createCell(cell).setCellValue(item.getValue().getString("summary"));
            }
            index++;
            // 从0列开始
            cell=0;
        }

        OutputStream out=new FileOutputStream("F:\\interface.xls");
        workbook.write(out);
        System.out.println(paths.toJSONString());
        return;
    }

    private String replace(String str){
       return str.replaceAll("\\{([^}]*)\\}","{*}");
    }
}
